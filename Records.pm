package XML::Records;
use strict;
use vars qw($VERSION);
$VERSION = '0.01';

use Carp;
use IO::File;
use XML::Parser;

sub new {
  my $class=shift;
  my $source=shift;
  my %args=(Latin=>0,Catalog=>0,@_);
  my $self={output=>[],EOF=>0,rectypes=>{}};
  $self->{latin}=delete $args{Latin};
  my $catname=delete $args{Catalog};
  my $parser=XML::Parser->new(%args) or croak "$!";
  $parser->setHandlers(Start=>\&start,End=>\&end,Char=>\&char);
  if ($catname) {
    require XML::Catalog;
    my $catalog=XML::Catalog->new($catname) or croak "$!";
    $parser->setHandlers(ExternEnt=>$catalog->get_handler($parser));
  }
  $self->{parser}=$parser->parse_start(Records=>$self) or croak "$!";  
  if (ref($source) eq 'SCALAR') {
    $self->{src}=$source;
    $self->{src_offset}=0;
  }
  elsif (ref($source)=~/^IO:|^GLOB$/) {
    $self->{srcfile}=$source;
  }
  else {
    $self->{srcfile}=IO::File->new($source,'r') or return undef;
    $self->{opened}=1;
  }
  bless $self,$class;
}

sub DESTROY {
  my $self=shift;
  $self->{srcfile}->close() if $self->{opened};
  $self->{parser}=undef;
}

sub set_records {
  my $self=shift;
  $self->{rectypes}={map {$_=>1} @_};
}

sub get_record {
  my $self=shift;
  my ($rec,$rectype);
  $self->{saverectypes}=$self->{rectypes};
  $self->set_records(@_) if @_;
  while (my $evt=$self->next_evt()) {
    next unless $evt->[0] eq 's';
    my $rt=$evt->[1];
    next unless $self->{rectypes}{$rt};
    $rec=$self->get_hash($evt);
    $rectype=$rt;
    last;
  }
  $self->{rectypes}=$self->{saverectypes};
  ($rectype,$rec);
}

sub get_hash {
  my ($self,$evt)=@_;
  my ($field,$buf,$field_evt);
  my $rectype=$evt->[1];
  my $nest=0;
  my $h={};
  # treat attributes of record or subrecord as fields
  for (my $i=2; $i<@$evt; $i+=2) {
    $h->{$evt->[$i]}=$evt->[$i+1];
  }
  while ($evt=$self->next_evt()) {
    my $t=$evt->[0];
    if ($t eq 's') {
      if ($nest++) { # start tag inside field, get subrecord
        $self->pushback($evt);
        add_hash($h,$field,$self->get_hash($field_evt));
        $nest-=2; # we won't see sub-field's or field's end tag
      }
      else {
        $buf="";
        $field=$evt->[1];
        $field_evt=$evt;
      }
    }
    elsif ($t eq 't') {
      $buf=$evt->[1] unless $evt->[1] =~ /^\s*$/;
    }
    else { # must be end tag
      last if $evt->[1] eq $rectype;
      add_hash($h,$field,$buf);
      --$nest;
    }
  }
  $h;
}

sub add_hash {
  my ($h,$field,$val)=@_;
  if (defined $h->{$field}) { # duplicate fields become arrays
    my $t=$h->{$field};
    $t=[$t] unless ref $t eq 'ARRAY';
    push @$t,$val;
    $val=$t;
  }
  $h->{$field}=$val;
}


sub next_evt {
  my $self=shift;
  my $buf;
  while (!@{$self->{output}} && !$self->{EOF}) {
    if (exists $self->{src}) {
      $buf=substr(${$self->{src}},$self->{src_offset},4096);
      $self->{src_offset}+=4096;
    }
    else {
      read($self->{srcfile},$buf,4096);
    }
    if (length($buf)==0) {
      $self->{EOF}=1;
      $self->{parser}->parse_done();
    }
    else {
      $self->{parser}->parse_more($buf);
    }
  }
  return shift(@{$self->{output}});
}

sub pushback {
  my ($self,$evt)=@_;
  unshift @{$self->{output}},$evt;
}

sub start {
  my ($parser,$element,@attrs)=@_;
  my $self=$parser->{Records};
  push @{$self->{output}},['s',$self->nsname($element)];
  while (@attrs) {
    my ($name,$val)=(shift @attrs,shift @attrs);
    push @{$self->{output}[-1]},$self->nsname($name),$self->encode($val);
  }
}

sub end {
  my ($parser,$element)=@_;
  my $self=$parser->{Records};
  push @{$self->{output}},['e',$self->nsname($element)];
}

sub char {
  my ($parser,$text)=@_;
  my $self=$parser->{Records};
  unless (@{$self->{output}} && $self->{output}[-1][0] eq 't') {
    push @{$self->{output}},['t',""];
  }
  $self->{output}[-1][1].=$self->encode($text);
}

sub nsname {
  my ($self,$name)=@_;
  my $parser=$self->{parser};
  if ($parser->{Namespaces}) {
    my $ns=$parser->namespace($name)||'';
    $name="{$ns}$name";
  }
  return $self->encode($name);    
}

sub encode {
  my ($self,$text)=@_;
  if ($self->{latin}) {
    $text=~s{([\xc0-\xc3])(.)}{
      my $hi = ord($1);
      my $lo = ord($2);
      chr((($hi & 0x03) <<6) | ($lo & 0x3F))
     }ge;
  }
  return $text;
}
1;
__END__

=head1 NAME

XML::Records - Perlish record-oriented interface to XML

=head1 SYNOPSIS

  use XML::Records;
  my $p=XML::Records->new('data.lst');
  $p->set_records('credit','debit');
  my ($t,$r)
  while ( (($t,$r)=$p->get_record()) && $t) {
    my $amt=$r->{Amount};
    if ($t eq 'debit') {
      ...
    }
  }

=head1 DESCRIPTION

XML::Records provides a simple interface for reading "record-structured" 
XML documents, that is, documents in which the immediate children of the 
root element form a sequence of identical and independent sub-elements such 
as log entries, transactions, etc., each of which consists of "field" child 
elements or attributes.  XML::Records allows you to access each record as a 
simple Perl hash.

=head1 METHODS

=over 4

=item $reader=XML::Records->new(source, [options]);

Creates a new reader object

I<source> is either a reference to a string containing the XML, the name of 
a file containing the XML, or an open IO::Handle or filehandle glob 
reference from which the XML can be read.

The I<Option>s can be any options allowed by XML::Parser and 
XML::Parser::Expat, as well as two module-specific options:

=over 4

=item I<Latin>

If set to a true value, causes Unicode characters in the range 128-255 to 
be returned as ISO-Latin-1 characters rather than UTF-8 characters.

=item I<Catalog>

Specifies the URL of a catalog to use for resolving public identifiers and 
remapping system identifiers used in document type declarations or external 
entity references.  This option requires XML::Catalog to be installed.

=back

=item $reader->set_records(name [,name]*);

Specifies what XML element-type names enclose records.

=item ($type,$record)=$reader->get_record([name [,name]*]);

Retrieves the next record from the input, skipping through the XML input 
until it encounters a start tag for one of the elements that enclose 
records.  If arguments are given, they will temporarily replace the set of 
record-enclosing elements.  The method will return a list consisting of the 
name of the record's enclosing element and a reference to a hash whose keys 
are the names of the record's child elements ("fields") and whose values 
are the fields' contents (if called in scalar context, the return value 
will be the hash reference).  Both elements of the list will be undef if no 
record can be found.

If a field's content is plain text, its value will be that text.

If a field's content contains another element (e.g. a <customer> record 
contains an <address> field that in turn contains other fields), its value 
will be a reference to another hash containing the "sub-record"'s fields.

If a record includes repeated fields, the hash entry for that field's 
name will be a reference to an array of field values.

Attributes of record or sub-record elements are treated as if they were 
fields.  Attributes of field elements are ignored.  Mixed content (fields 
with both non-whitespace text and sub-elements) will lead to unpredictable 
results.

Records do not actually need to be immediately below the document 
root.  If a <customers> document consists of a sequence of <customer> 
elements which in turn contain <address> elements that include further 
elements, then calling get_record with the record type set to "address" 
will return the contents of each <address> element.

=back

=head1 EXAMPLE

Print a list of package names from a (rather out-of-date) list of XML 
modules:

 #!perl -w
 use strict;
 use XML::Records;
 
 my $p=XML::Records->new('modules.xml') or die "$!";
 $p->set_records('module');
 while (my $record=$p->get_record()) {
   my $pkg=$record->{package};
   if (ref $pkg eq 'ARRAY') {
     for my $subpkg (@$pkg) {
       print $subpkg->{name},"\n";
     }
   }
   else {
     print $pkg->{name},"\n";
   }
 }

=head1 RATIONALE

XML::RAX, which implements the proposed RAX standard for record-oriented 
XML access, does most of what XML::Records does, but its interface is not 
very Perlish (due to the fact that RAX is a language-independent interface) 
and it cannot cope with fields that have sub-structure (because RAX itself 
doesn't address the issue).

XML::Simple can do everything that XML::Records does, at the expense of 
reading the entire document into memory.  XML::Records will read the entire 
document into a single hash if you set the root element as a record type, 
but you're really better off using XML::Simple in that case as it's 
optimized for such usage.

=head1 AUTHOR

Eric Bohlman (ebohlman@earthlink.net, ebohlman@omsdev.com)

=head1 COPYRIGHT

Copyright 2001 Eric Bohlman.  All rights reserved.

This program is free software; you can use/modify/redistribute it under the
same terms as Perl itself.

=head1 SEE ALSO

  XML::Parser
  XML::RAX
  XML::Simple
  XML::Catalog
  perl(1).

=cut


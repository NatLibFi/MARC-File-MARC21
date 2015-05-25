package MARC::File::MARC21;

=head1 NAME

MARC::File::MARC21 - MARC21 (ISO 2709) format-specific file handling. 

=cut

use strict;
use warnings;
use integer;

use vars qw( $ERROR );
#	use MARC::File::Encode qw( marc_to_utf8 );

use MARC::File;
use vars qw( @ISA ); @ISA = qw( MARC::File );

use MARC::Record;
use MARC::Field;
use Carp;

use constant SUBFIELD_INDICATOR     => "\x1F";
use constant END_OF_FIELD           => "\x1E";
use constant END_OF_RECORD          => "\x1D";
use constant DIRECTORY_ENTRY_LEN    => 12;
use constant LEADER_LEN 	        => 24;

our $VERSION = "1.0.0";
our $conversion;
our $onEncode;
=head1 SYNOPSIS



=head1 EXPORT

None.

=head1 METHODS

=cut

=head2 $MARC::File::MARC21::conversion

If $MARC::File::MARC21::conversion is a function, then it's called at the data being read.
Useful f.ex. if you want to convert the charset into something more suitable.
Function is called with a single parameter: the unparsed data read from the file.

Example:

#note: marc8_to_utf8 is from module called MARC::Charset

sub convert_marc8_to_utf8 {
	my $data = shift;
		
	$data = Unicode::Normalize::compose( marc8_to_utf8($data) );
		
	return $data;
	
}

$MARC::File::MARC21::conversion = \&convert_marc8_to_utf8;

my $file = MARC::File::MARC21->in( $config->{'infile'} );

while ( my $marc = $file->next() ) {

	#do something with $marc (single MARC::Record) that has been converted from marc8 to utf8
}


=cut


=head2 $MARC::File::MARC21::onEncode

If $MARC::File::MARC21::onEncode is a function, then it's called for each field's data before in encode function.
Useful f.ex. if you want to convert the charset into something more suitable.
Function is called with a single parameter: the data part of the field.

Example:

#note: utf8_to_marc8 is from module called MARC::Charset

sub convert_utf8_to_marc8 {
	my $data = shift;
		
	$data = Unicode::Normalize::compose( convert_utf8_to_marc8($data) );
		
	return $data;
	
}

$MARC::File::MARC21::onEncode = \&convert_utf8_to_marc8;

=cut


sub _next { ## no critic (ProhibitUnusedPrivateSubroutines)
    my $self = shift;
    my $fh = $self->{fh};

    my $reclen;
    return if eof($fh);

    local $/ = END_OF_RECORD;
    my $rec = <$fh>;

	if(defined($conversion)) {
		$rec = $conversion->($rec);
	}

    # remove illegal garbage that sometimes occurs between records
    # $rec =~ s/^[ \x0a]+//;
    
    return $rec;
}





=head2 decode( $string [, \&filter_func ] )

Constructor for handling data from a MARC21 file.  This function takes care of
all the tag directory parsing & mangling.

Any warnings or coercions can be checked in the C<warnings()> function.

The C<$filter_func> is an optional reference to a user-supplied function
that determines on a tag-by-tag basis if you want the tag passed to it
to be put into the MARC record.  The function is passed the tag number
and the raw tag data, and must return a boolean.  The return of a true
value tells MARC::File::MARC21::decode that the tag should get put into
the resulting MARC record.

For example, if you only want title and subject tags in your MARC record,
try this:

    sub filter {
        my ($tagno,$tagdata) = @_;

        return ($tagno == 245) || ($tagno >= 600 && $tagno <= 699);
    }

    my $marc = MARC::File::MARC21->decode( $string, \&filter );

Why would you want to do such a thing?  The big reason is that creating
fields is processor-intensive, and if your program is doing read-only
data analysis and needs to be as fast as possible, you can save time by
not creating fields that you'll be ignoring anyway.

Another possible use is if you're only interested in printing certain
tags from the record, then you can filter them when you read from disc
and not have to delete unwanted tags yourself.


Do not give utf8 encoded strings to this method.
=cut


sub decode ($$;$) {

    my $text;
    my $location = '';

    ## decode can be called in a variety of ways
    ## $object->decode( $string )
    ## MARC::File::MARC21->decode( $string )
    ## MARC::File::MARC21::decode( $string )
    ## this bit of code covers all three

    my $self = shift;
    if ( ref($self) =~ /^MARC::File/ ) {
        $location = 'in record '.$self->{recnum};
        $text = shift;
    } else {
        $location = 'in record 1';
        $text = $self=~/MARC::File/ ? shift : $self;
    }
    my $filter_func = shift;



    # ok this the empty shell we will fill
    my $marc = MARC::Record->new();

    # Check for an all-numeric record length
    ($text =~ /^(\d{5})/)
        or return $marc->_warn( "Record length \"", substr( $text, 0, 5 ), "\" is not numeric $location" );

    my $reclen = $1;
    my $realLength = bytes::length( $text );
  
    $marc->_warn( "Invalid record length $location: Leader says $reclen " . 
        "bytes but it's actually $realLength" ) unless $reclen == $realLength;

    (substr($text, -1, 1) eq END_OF_RECORD)
        or $marc->_warn( "Invalid record terminator $location" );

    $marc->leader( substr( $text, 0, LEADER_LEN ) );

    # bytes 12 - 16 of leader give offset to the body of the record
    my $data_start = 0 + bytes::substr( $text, 12, 5 );

    # immediately after the leader comes the directory (no separator)
    my $dir = substr( $text, LEADER_LEN, $data_start - LEADER_LEN - 1 );  # -1 to allow for \x1e at end of directory

    # character after the directory must be \x1e
    (substr($text, $data_start-1, 1) eq END_OF_FIELD)
        or $marc->_warn( "No directory found $location" );

    # all directory entries 12 bytes long, so length % 12 must be 0
    (length($dir) % DIRECTORY_ENTRY_LEN == 0)
        or $marc->_warn( "Invalid directory length $location" );


    # go through all the fields
    my $nfields = length($dir)/DIRECTORY_ENTRY_LEN;
    for ( my $n = 0; $n < $nfields; $n++ ) {
        my ( $tagno, $len, $offset ) = unpack( "A3 A4 A5", substr($dir, $n*DIRECTORY_ENTRY_LEN, DIRECTORY_ENTRY_LEN) );

        # Check directory validity
        ($tagno =~ /^[0-9A-Za-z]{3}$/)
            or $marc->_warn( "Invalid tag in directory $location: \"$tagno\"" );

        ($len =~ /^\d{4}$/)
            or $marc->_warn( "Invalid length in directory $location tag $tagno: \"$len\"" );

        ($offset =~ /^\d{5}$/)
            or $marc->_warn( "Invalid offset in directory $location tag $tagno: \"$offset\"" );

        ($offset + $len <= $reclen)
            or $marc->_warn( "Directory entry $location runs off the end of the record tag $tagno" );

        my $tagdata = bytes::substr( $text, $data_start+$offset, $len ); 

   
        $marc->_warn( "Invalid length in directory for tag $tagno $location" )
            unless ( $len == bytes::length($tagdata) );

        if ( substr($tagdata, -1, 1) eq END_OF_FIELD ) {
            # get rid of the end-of-tag character
            chop $tagdata;
            --$len;
        } else {
            $marc->_warn( "field does not end in end of field character in tag $tagno $location" );
        }

        warn "Specs: ", join( "|", $tagno, $len, $offset, $tagdata ), "\n" if $MARC::Record::DEBUG;

        if ( $filter_func ) {
            next unless $filter_func->( $tagno, $tagdata );
        }

	

        if ( MARC::Field->is_controlfield_tag($tagno) ) {
            $marc->append_fields( MARC::Field->new( $tagno, $tagdata ) );
        } else {
            my @subfields = split( SUBFIELD_INDICATOR, $tagdata );
            my $indicators = shift @subfields;
            my ($ind1, $ind2);

            if ( length( $indicators ) > 2 or length( $indicators ) == 0 ) {
                $marc->_warn( "Invalid indicators \"$indicators\" forced to blanks $location for tag $tagno\n" );
                ($ind1,$ind2) = (" ", " ");
            } else {
                $ind1 = substr( $indicators,0, 1 );
                $ind2 = substr( $indicators,1, 1 );
            }

            # Split the subfield data into subfield name and data pairs
            my @subfield_data;
            for ( @subfields ) {
                if ( length > 0 ) {
                    push( @subfield_data, substr($_,0,1),substr($_,1) );
                } else {
                    $marc->_warn( "Entirely empty subfield found in tag $tagno" );
                }
            }

            if ( !@subfield_data ) {
                $marc->_warn( "no subfield data found $location for tag $tagno" );
                next;
            }

            my $field = MARC::Field->new($tagno, $ind1, $ind2, @subfield_data );
            if ( $field->warnings() ) {
                $marc->_warn( $field->warnings() );
            }
            $marc->append_fields( $field );
        }
    } # looping through all the fields


    return $marc;
}




sub get_xml_text {
  my ($node) = @_;

  return '' if (!$node);

  $node = $node->getFirstChild();
  return '' if (!$node);

  my $str = $node->getData();
  return pack('C*', unpack('U0C*', $str));
}



=head2 _build_tag_directory()

Function for internal use only: Builds the tag directory that gets
put in front of the data in a MARC record.

Returns two array references, and two lengths: The tag directory, and the data fields themselves,
the length of all data (including the Leader that we expect will be added),
and the size of the Leader and tag directory.

=cut

sub _build_tag_directory {  ## no critic (ProhibitUnusedPrivateSubroutines)
        my $marc = shift;
        $marc = shift if (ref($marc)||$marc) =~ /^MARC::File/;
        croak "Wanted a MARC::Record but got a ", ref($marc) unless ref($marc) eq "MARC::Record";

        my @fields;
        my @directory;

        my $dataend = 0;
        for my $field ( $marc->fields() ) {
                # Dump data into proper format
                my $str = $field->as_usmarc;
                push( @fields, $str );

                # Create directory entry
                my $len = bytes::length( $str );

                my $direntry = sprintf( "%03s%04d%05d", $field->tag, $len, $dataend );
                push( @directory, $direntry );
                $dataend += $len;
        }

        my $baseaddress =
                LEADER_LEN +    # better be 24
                ( @directory * DIRECTORY_ENTRY_LEN ) +
                                # all the directory entries
                1;              # end-of-field marker


        my $total =
                $baseaddress +  # stuff before first field
                $dataend +      # Length of the fields
                1;              # End-of-record marker



        return (\@fields, \@directory, $total, $baseaddress);
}

=head2 encode()

Returns a string of characters suitable for writing out to a MARC21 format file

=cut

sub encode($$;$) {
    my $marc = shift;
    $marc = shift if (ref($marc)||$marc) =~ /^MARC::File/;

	my @list = ();
	my $field_start = "\x1f";
	my $field_end = "\x1e";
  
  
    push(@list, {'code' => '000', 'data' => $marc->leader()});
    
    
	for my $field ($marc->fields()) {
		
		if ($field->is_control_field()) {
			
			my $tag = $field->tag();
			my $fielddata = $field->data();
			$fielddata =~ s/\r\n/ /g;
			$fielddata =~ s/\r//g;
			$fielddata =~ s/\n/ /g;
		
			if ($tag eq 'LDR')
			{
			  $tag = '000';
			  $fielddata = justifyleftch($fielddata, 20, ' ') . '4500';
			}
			
			
			push(@list, {'code' => $tag, 'data' => $fielddata});
			
		} else {
			my $tag = $field->tag();
			my $fielddata = $field->indicator(1) . $field->indicator(2);
			
			for my $subfield ($field->subfields) {
				
				  my $sub_code = $subfield->[0];
				  my $sub_contents = $subfield->[1];
				  $sub_contents =~ s/\r\n/ /g;
				  $sub_contents =~ s/\r//g;
				  $sub_contents =~ s/\n/ /g;
			
			
				$fielddata .= "$field_start$sub_code$sub_contents";
			}
			$fielddata .= $field_end;
			
			 push(@list, {'code' => $tag, 'data' => $fielddata});
		}
		
		
	}

 
	return list_to_marc(\@list);

}

sub justifyleftch {
  my ($str, $len, $padch) = @_;

  $str = substr($str, 0, $len);
  while (length($str) < $len)
  {
      $str = $str . $padch;
  }
  return $str;
}

sub justifyrightch {
  my ($str, $len, $padch) = @_;

  $str = substr($str, 0, $len);
  while (length($str) < $len)
  {
      $str = $padch . $str;
  }
  return $str;
}

sub list_to_marc {
  my ($a_list) = @_;

  my $leader = '';
  my $directory = '';
  my $marcdata = '';
  my $datapos = 0;

  my $field_start = "\x1f";
  my $field_end = "\x1e";
  my $record_end = "\x1d";

  my $fields = scalar(@$a_list);
  for (my $i = 0; $i < $fields; $i++)
  {
    my $code = $a_list->[$i]{'code'};
    my $fielddata = $a_list->[$i]{'data'};
    if ($code eq '000')
    {
      $leader = $fielddata;
      $leader = justifyleftch($leader, 24, '0');
      next;
    }
    next if ($code !~ /^\d{3}$/);

	if(defined($onEncode)) {
		$fielddata = $onEncode->($fielddata);
	}
 
    $fielddata .= $field_end if (substr($fielddata, length($fielddata) - 1, 1) ne $field_end);

    $directory .= justifyrightch($code, 3, '0') . justifyrightch(length($fielddata), 4, '0') .
          justifyrightch($datapos, 5, '0');

    $marcdata .= $fielddata;
    $datapos += length($fielddata);
  }
  $directory .= $field_end;
  $marcdata .= $record_end;

  croak('Internal error: leader empty') if (!$leader);
  
  my $len = length($leader) + length($directory) + length($marcdata);
  my $datastart = length($leader) + length($directory);
  $leader = justifyrightch($len, 5, '0') . substr($leader, 5, 7) . justifyrightch($datastart, 5, '0') .
    substr($leader, 17, length($leader));

  return "$leader$directory$marcdata";
}



1;

__END__

=head1 RELATED MODULES

L<MARC::Record>

=head1 TODO


=head1 LICENSE

This code may be distributed under the same terms as Perl itself.

Please note that these modules are not products of or supported by the
employers of the various contributors to the code.

=head1 AUTHOR

Pasi Tuominen, pasi.e.tuominen@helsinki.fi

=cut


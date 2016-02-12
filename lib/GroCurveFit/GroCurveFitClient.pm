package GroCurveFit::GroCurveFitClient;

use JSON::RPC::Client;
use POSIX;
use strict;
use Data::Dumper;
use URI;
use Bio::KBase::Exceptions;
my $get_time = sub { time, 0 };
eval {
    require Time::HiRes;
    $get_time = sub { Time::HiRes::gettimeofday() };
};

use Bio::KBase::AuthToken;

# Client version should match Impl version
# This is a Semantic Version number,
# http://semver.org
our $VERSION = "0.1.0";

=head1 NAME

GroCurveFit::GroCurveFitClient

=head1 DESCRIPTION


A KBase module: GroCurveFit
This module contains one small method - fit_growth_curve.


=cut

sub new
{
    my($class, $url, @args) = @_;
    

    my $self = {
	client => GroCurveFit::GroCurveFitClient::RpcClient->new,
	url => $url,
	headers => [],
    };

    chomp($self->{hostname} = `hostname`);
    $self->{hostname} ||= 'unknown-host';

    #
    # Set up for propagating KBRPC_TAG and KBRPC_METADATA environment variables through
    # to invoked services. If these values are not set, we create a new tag
    # and a metadata field with basic information about the invoking script.
    #
    if ($ENV{KBRPC_TAG})
    {
	$self->{kbrpc_tag} = $ENV{KBRPC_TAG};
    }
    else
    {
	my ($t, $us) = &$get_time();
	$us = sprintf("%06d", $us);
	my $ts = strftime("%Y-%m-%dT%H:%M:%S.${us}Z", gmtime $t);
	$self->{kbrpc_tag} = "C:$0:$self->{hostname}:$$:$ts";
    }
    push(@{$self->{headers}}, 'Kbrpc-Tag', $self->{kbrpc_tag});

    if ($ENV{KBRPC_METADATA})
    {
	$self->{kbrpc_metadata} = $ENV{KBRPC_METADATA};
	push(@{$self->{headers}}, 'Kbrpc-Metadata', $self->{kbrpc_metadata});
    }

    if ($ENV{KBRPC_ERROR_DEST})
    {
	$self->{kbrpc_error_dest} = $ENV{KBRPC_ERROR_DEST};
	push(@{$self->{headers}}, 'Kbrpc-Errordest', $self->{kbrpc_error_dest});
    }

    #
    # This module requires authentication.
    #
    # We create an auth token, passing through the arguments that we were (hopefully) given.

    {
	my $token = Bio::KBase::AuthToken->new(@args);
	
	if (!$token->error_message)
	{
	    $self->{token} = $token->token;
	    $self->{client}->{token} = $token->token;
	}
        else
        {
	    #
	    # All methods in this module require authentication. In this case, if we
	    # don't have a token, we can't continue.
	    #
	    die "Authentication failed: " . $token->error_message;
	}
    }

    my $ua = $self->{client}->ua;	 
    my $timeout = $ENV{CDMI_TIMEOUT} || (30 * 60);	 
    $ua->timeout($timeout);
    bless $self, $class;
    #    $self->_validate_version();
    return $self;
}




=head2 fit_growth_curve

  $output_object = $obj->fit_growth_curve($workspace_name, $growth_matrix_id, $parameters_obj_name, $fit_method)

=over 4

=item Parameter and return types

=begin html

<pre>
$workspace_name is a GroCurveFit.workspace_name
$growth_matrix_id is a GroCurveFit.growth_matrix_id
$parameters_obj_name is a GroCurveFit.parameters_obj_name
$fit_method is a string
$output_object is a string
workspace_name is a string
growth_matrix_id is a string
parameters_obj_name is a string

</pre>

=end html

=begin text

$workspace_name is a GroCurveFit.workspace_name
$growth_matrix_id is a GroCurveFit.growth_matrix_id
$parameters_obj_name is a GroCurveFit.parameters_obj_name
$fit_method is a string
$output_object is a string
workspace_name is a string
growth_matrix_id is a string
parameters_obj_name is a string


=end text

=item Description

Returns growth matrix parameters
growth_matrix_id - the GrowthMatrix to fit.

=back

=cut

 sub fit_growth_curve
{
    my($self, @args) = @_;

# Authentication: required

    if ((my $n = @args) != 4)
    {
	Bio::KBase::Exceptions::ArgumentValidationError->throw(error =>
							       "Invalid argument count for function fit_growth_curve (received $n, expecting 4)");
    }
    {
	my($workspace_name, $growth_matrix_id, $parameters_obj_name, $fit_method) = @args;

	my @_bad_arguments;
        (!ref($workspace_name)) or push(@_bad_arguments, "Invalid type for argument 1 \"workspace_name\" (value was \"$workspace_name\")");
        (!ref($growth_matrix_id)) or push(@_bad_arguments, "Invalid type for argument 2 \"growth_matrix_id\" (value was \"$growth_matrix_id\")");
        (!ref($parameters_obj_name)) or push(@_bad_arguments, "Invalid type for argument 3 \"parameters_obj_name\" (value was \"$parameters_obj_name\")");
        (!ref($fit_method)) or push(@_bad_arguments, "Invalid type for argument 4 \"fit_method\" (value was \"$fit_method\")");
        if (@_bad_arguments) {
	    my $msg = "Invalid arguments passed to fit_growth_curve:\n" . join("", map { "\t$_\n" } @_bad_arguments);
	    Bio::KBase::Exceptions::ArgumentValidationError->throw(error => $msg,
								   method_name => 'fit_growth_curve');
	}
    }

    my $result = $self->{client}->call($self->{url}, $self->{headers}, {
	method => "GroCurveFit.fit_growth_curve",
	params => \@args,
    });
    if ($result) {
	if ($result->is_error) {
	    Bio::KBase::Exceptions::JSONRPC->throw(error => $result->error_message,
					       code => $result->content->{error}->{code},
					       method_name => 'fit_growth_curve',
					       data => $result->content->{error}->{error} # JSON::RPC::ReturnObject only supports JSONRPC 1.1 or 1.O
					      );
	} else {
	    return wantarray ? @{$result->result} : $result->result->[0];
	}
    } else {
        Bio::KBase::Exceptions::HTTP->throw(error => "Error invoking method fit_growth_curve",
					    status_line => $self->{client}->status_line,
					    method_name => 'fit_growth_curve',
				       );
    }
}
 
  

sub version {
    my ($self) = @_;
    my $result = $self->{client}->call($self->{url}, $self->{headers}, {
        method => "GroCurveFit.version",
        params => [],
    });
    if ($result) {
        if ($result->is_error) {
            Bio::KBase::Exceptions::JSONRPC->throw(
                error => $result->error_message,
                code => $result->content->{code},
                method_name => 'fit_growth_curve',
            );
        } else {
            return wantarray ? @{$result->result} : $result->result->[0];
        }
    } else {
        Bio::KBase::Exceptions::HTTP->throw(
            error => "Error invoking method fit_growth_curve",
            status_line => $self->{client}->status_line,
            method_name => 'fit_growth_curve',
        );
    }
}

sub _validate_version {
    my ($self) = @_;
    my $svr_version = $self->version();
    my $client_version = $VERSION;
    my ($cMajor, $cMinor) = split(/\./, $client_version);
    my ($sMajor, $sMinor) = split(/\./, $svr_version);
    if ($sMajor != $cMajor) {
        Bio::KBase::Exceptions::ClientServerIncompatible->throw(
            error => "Major version numbers differ.",
            server_version => $svr_version,
            client_version => $client_version
        );
    }
    if ($sMinor < $cMinor) {
        Bio::KBase::Exceptions::ClientServerIncompatible->throw(
            error => "Client minor version greater than Server minor version.",
            server_version => $svr_version,
            client_version => $client_version
        );
    }
    if ($sMinor > $cMinor) {
        warn "New client version available for GroCurveFit::GroCurveFitClient\n";
    }
    if ($sMajor == 0) {
        warn "GroCurveFit::GroCurveFitClient version is $svr_version. API subject to change.\n";
    }
}

=head1 TYPES



=head2 growth_matrix_id

=over 4



=item Description

A string representing a GrowthMatrix id.


=item Definition

=begin html

<pre>
a string
</pre>

=end html

=begin text

a string

=end text

=back



=head2 workspace_name

=over 4



=item Description

A string representing a workspace name.


=item Definition

=begin html

<pre>
a string
</pre>

=end html

=begin text

a string

=end text

=back



=head2 parameters_obj_name

=over 4



=item Description

A string representing final object name


=item Definition

=begin html

<pre>
a string
</pre>

=end html

=begin text

a string

=end text

=back



=head2 GrowthCurveParameters

=over 4



=item Description

Parameters of a single growth curve


=item Definition

=begin html

<pre>
a reference to a hash where the following keys are defined:
mtx_column_id has a value which is a string
method has a value which is a string
growth_rate has a value which is a float
lag_phase has a value which is a float
max_growth has a value which is a float
area_under_curve has a value which is a float

</pre>

=end html

=begin text

a reference to a hash where the following keys are defined:
mtx_column_id has a value which is a string
method has a value which is a string
growth_rate has a value which is a float
lag_phase has a value which is a float
max_growth has a value which is a float
area_under_curve has a value which is a float


=end text

=back



=head2 GrowthParameters

=over 4



=item Description

Parameters of all growth curves for a GrowthMatrix


=item Definition

=begin html

<pre>
a reference to a hash where the following keys are defined:
matrix_id has a value which is a string
parameters has a value which is a reference to a list where each element is a GroCurveFit.GrowthCurveParameters

</pre>

=end html

=begin text

a reference to a hash where the following keys are defined:
matrix_id has a value which is a string
parameters has a value which is a reference to a list where each element is a GroCurveFit.GrowthCurveParameters


=end text

=back



=cut

package GroCurveFit::GroCurveFitClient::RpcClient;
use base 'JSON::RPC::Client';
use POSIX;
use strict;

#
# Override JSON::RPC::Client::call because it doesn't handle error returns properly.
#

sub call {
    my ($self, $uri, $headers, $obj) = @_;
    my $result;


    {
	if ($uri =~ /\?/) {
	    $result = $self->_get($uri);
	}
	else {
	    Carp::croak "not hashref." unless (ref $obj eq 'HASH');
	    $result = $self->_post($uri, $headers, $obj);
	}

    }

    my $service = $obj->{method} =~ /^system\./ if ( $obj );

    $self->status_line($result->status_line);

    if ($result->is_success) {

        return unless($result->content); # notification?

        if ($service) {
            return JSON::RPC::ServiceObject->new($result, $self->json);
        }

        return JSON::RPC::ReturnObject->new($result, $self->json);
    }
    elsif ($result->content_type eq 'application/json')
    {
        return JSON::RPC::ReturnObject->new($result, $self->json);
    }
    else {
        return;
    }
}


sub _post {
    my ($self, $uri, $headers, $obj) = @_;
    my $json = $self->json;

    $obj->{version} ||= $self->{version} || '1.1';

    if ($obj->{version} eq '1.0') {
        delete $obj->{version};
        if (exists $obj->{id}) {
            $self->id($obj->{id}) if ($obj->{id}); # if undef, it is notification.
        }
        else {
            $obj->{id} = $self->id || ($self->id('JSON::RPC::Client'));
        }
    }
    else {
        # $obj->{id} = $self->id if (defined $self->id);
	# Assign a random number to the id if one hasn't been set
	$obj->{id} = (defined $self->id) ? $self->id : substr(rand(),2);
    }

    my $content = $json->encode($obj);

    $self->ua->post(
        $uri,
        Content_Type   => $self->{content_type},
        Content        => $content,
        Accept         => 'application/json',
	@$headers,
	($self->{token} ? (Authorization => $self->{token}) : ()),
    );
}



1;

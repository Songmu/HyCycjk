package HyCycjk;
use strict;
use warnings;
use utf8;
require Encode;

our $VERSION = '0.01';
our $CURRENT_CLASS = '';
our %DISPATCH_RULES;

our $MAX_POST_BODY_SIZE = 10000000;
our $DEBUG              = 1;
our $NOT_FOUND_CODE     = +{
    headers => +{ Status => 404 },
    body    => 'Not Found',
};
our $STATIC_FILE_PATH = '';
our $CONTROLLER_PATH = '';
our $VIEW_PATH = '';
our $WORK_PATH = './work/';
our $LOG_PATH = './logs/';

our $APP_CONFIG;

our $MEDIA_MIMETYPES = +{
    jpg     => 'image/jpeg',
    gif     => 'image/gif',
    png     => 'image/png',
    svg     => 'image/svg+xml',
    pdf     => 'application/pdf',
    swf     => 'application/x-shockwave-flash',
    ico     => 'image/vnd.microsoft.icon',
};
our $STATIC_MIMETYPES = {
    css     => 'text/css',
    js      => 'text/javascript',
    html    => 'text/html',
    xml     => 'text/xml',
    txt     => 'text/plain',
};

our $TEMPLATE_MIMETYPES = +{
    html    => 'text/html; charset=utf-8',
    mt      => 'text/html; charset=utf-8',
    css     => 'text/css',
    js      => 'text/javascript',
    xml     => 'text/xml',
    default => 'text/plain',
};

our $TEMPLATE_PARAMS = +{
    code                => '',
    comment_mark        => '#',
    expression_mark     => '=',
    raw_expression_mark => '=r',
    line_start          => '?',
    template            => '',
    tag_start           => '<?',
    tag_end             => '?>',
    prefix              => '',
    ext                 => 'mt',
};

my $CRLF  = "\r\n";
my $_GET = undef;
my $_POST = undef;
my $_SESSION = undef;

sub import {
    my ( $class, %args ) = @_;
    $_GET  = undef;
    $_POST = undef;
    my $caller = caller;
    $CURRENT_CLASS = $args{current_class} || $caller;
    
    # functions export
    no strict 'refs';
    for my $name (qw/ dispatch query post_param query_keys post_keys controller model view redirect filter view_template run is_post_request dispatch_rules logging/)
    {
        *{ $caller . '::' . $name } = \&{$name};
    }
    for my $meth (qw/get set keys remove as_hashref expire regenerate_session_id session_id/) {
        *{ __PACKAGE__ . '::'. "session_$meth"} = \&{"session_$meth"};
    }
    strict->import;
    warnings->import;
    utf8->import;
}

sub read_file {
    my $file = shift;
    open my $fh, '<', $file or die $file . ': ' . $!;
    do { local $/; <$fh> };
}

sub _read_binary {
    my $file = shift;
    open my $fh, '<', $file or die $file . ': ' . $!;
    binmode $fh;
    my $size = read( $fh, my $bin, -s $file );
    return ( $bin, $size );
}

sub dispatch {
    my $conf = shift;
    if ( $conf ){
        if ( ref $conf->{'hycycjk'} eq 'HASH' ){
            no strict 'refs';
            while ( my ($key, $val) = each %{$conf->{'hycycjk'}} ){
                $HyCycjk::{$key} = $val;
            }
        }
        if ( ref $conf->{'application'} eq 'HASH' ){
            $HyCycjk::APP_CONFIG = $conf->{'application'};
        }
    }
    my $response;
    my $caller_filename = ( caller(0) )[1];

    # copied by MENTA
    local $SIG{__DIE__} = sub { #error handler
        my $msg = shift;

        my $i = 0;
        my @trace;

        if ( ref $msg eq 'HASH' ){
            @trace = @{$msg->{trace}};
            $msg = $msg->{message};
        }
        else {
            while ( my ( $package, $filename, $line, ) = caller($i) ) {
                my $context = sub {
                    my ( $file, $linenum ) = @_;
                    my $code;
                    if ( -f $file ) {
                        my $start = $linenum - 3;
                        my $end   = $linenum + 3;
                        $start = $start < 1 ? 1 : $start;
                        open my $fh, '<:utf8', $file or die $file . ': ' . $!;
                        my $cur_line = 0;
                        while ( my $line = <$fh> ) {
                            ++$cur_line;
                            last if $cur_line > $end;
                            next if $cur_line < $start;
                            my @tag
                                = $cur_line == $linenum
                                ? ( q{<strong>}, '</strong>' )
                                : ( '', '' );
                            $code .= sprintf( '%s%5d: %s%s',
                                $tag[0], $cur_line, filter( $line => 'html' ),
                                $tag[1], );
                        }
                        close $file;
                    }
                    return $code;
                    }
                    ->( $filename, $line );
                $filename = 'CGI file' if $filename eq $caller_filename;
                push @trace,
                    +{
                    level    => $i,
                    package  => $package,
                    filename => $filename,
                    line     => $line,
                    context  => $context
                    };
                $i++;
            }
        }
        $msg =~ s/\Q$caller_filename/CGI file/g;
        die { message => $msg, trace => \@trace };
    };

    eval { #main process
        $_SESSION = HyCycjk::Session->new;
        my $action = lc( $ENV{PATH_INFO} || '' );
        $action =~ s!^/+!!  if( $action );
        die 'Bad Request' if $action =~ /\.\./; #for directory traversal

        ( $action, my @snippets ) = _get_ready_controller( $action );

        my $media_exts = join '|', map{quotemeta} keys %{$MEDIA_MIMETYPES};
        my $static_exts = join '|', map{quotemeta} keys %{$STATIC_MIMETYPES};
        if ( $action =~ /\.(?:$media_exts)$/ && -e $STATIC_FILE_PATH.$action ){ #静的ファイル対応
            $response = view_image( $STATIC_FILE_PATH.$action );
        }
        elsif ( $action =~ /\.(?:$static_exts)$/ && -e $STATIC_FILE_PATH.$action ){
            $response = view_static( $STATIC_FILE_PATH.$action );
        }
        elsif (-e $CONTROLLER_PATH.$action.'.pl' ){
            $response = require "$CONTROLLER_PATH$action.pl"; #ここでも@snippetsを渡す？
            $response = $response->(@snippets) if ref $response eq 'CODE';
        }
        else{
            $action = sub { #camelize
                return $_[0] if $_[0] !~ /_/;
                my $str = join '', map { ucfirst } split /_/ , shift;
                lcfirst $str;
            }->( $action );
            
            $action =~ s!/!_!g  if( $action );
            #ドットを指定しないと、静的ファイルが見つからなかった場合、indexに飛んでしまう(現状の苦肉の策)
            $action = '' unless $action =~ /^[a-zA-Z0-9_.]*$/;
            $action ||= 'index';

            my $method_prefix = 
                $ENV{REQUEST_METHOD} eq "POST" ? 'dopost_' : 'do_';
            my $func = $method_prefix . $action;
            if ( my $code = $CURRENT_CLASS->can($func) ) {
                $response = $code->(@snippets);
            }
            else{
                eval {
                    no strict 'refs';
                    $response = "$CURRENT_CLASS\::view"->(
                        sub { #uncamelize
                            return $_[0] if $_[0] !~ /[_A-Z]/;
                            my $str = shift;
                            $str =~ s!_!/!g;
                            $str =~ s!([A-Z])!'_'.lc($1)!eg;
                            $str;
                        }->($action),
                        @snippets
                    );
                };
                if ( $@ ){
                    die $@ unless $@->{'message'} =~
                        /^(?:template file not found|No such file or directory)/;
                    $response
                        = ref($NOT_FOUND_CODE) eq 'CODE'
                        ? $NOT_FOUND_CODE->($func)
                        : $NOT_FOUND_CODE;
                }
            }
        }
        $response = { body => $response } if !ref $response;
        die '$response must be a hashref!' if ref $response ne 'HASH';
    };
    if ($@) { #catch
        my $err = $@;
        die $err unless $DEBUG;
        die $err unless ref($err) eq 'HASH';

        warn $err->{message};

        my $body;
        my $msg = filter( $err->{message} => 'html' );
        $body
            = qq{<!doctype html><head><title>500 Internal Server Error</title><style type="text/css">body { margin: 0; padding: 0; background: rgb(230, 230, 230); color: rgb(44, 44, 44); } h1 { margin: 0 0 .5em; padding: .25em; border: 0 none; border-bottom: medium solid rgb(0, 0, 15); background: rgb(63, 63, 63); color: rgb(239, 239, 239); font-size: x-large; } p { margin: .5em 1em; } li { font-size: small; } pre { background: rgb(255, 239, 239); color: rgb(47, 47, 47); font-size: medium; } pre code strong { color: rgb(0, 0, 0); background: rgb(255, 143, 143); } p.f { text-align: right; font-size: xx-small; } p.f span { font-size: medium; }</style></head><h1>500 Internal Server Error</h1><p>$msg</p><ol>};
        for my $stack ( @{ $err->{trace} } ) {
            $body .= '<li>'
                . filter(
                join( ', ',
                    $stack->{package}, $stack->{filename}, $stack->{line} ) =>
                    'html'
                ) . qq(<pre><code>$stack->{context}</code></pre></li>);
        }
        $body
            .= qq{</ol><p class="f"><span>Powered by <strong>HyCycjk</strong></span>, Web application framework</p>};

        eval "use utf8"; # display errors
        utf8::encode($body);
        $response = +{
            headers => +{ Status => 500 },
            body    => $body,
        };
        die $@ unless $response;
    }

    $response ||= +{};
    my %headers = %{ $response->{headers} || +{} };
    my $body = $response->{body} || '';
    $headers{'Content-Length'} ||= length(Encode::encode_utf8($body));
    $headers{'Content-Type'}  ||= 'text/html; charset=utf-8';
    $headers{'Set-Cookie'} = $_SESSION->cookie_str if $_SESSION->is_fresh;
    $_SESSION->finalize;

    binmode STDOUT;
    # build headers
    while ( my ( $name, $values ) = each %headers ) {
        next unless defined $values;
        for my $value ( ref($values) eq 'ARRAY' ? @{$values} : ($values) ) {
            print STDOUT $name . ': ' . $value . $CRLF;
        }
    }
    print STDOUT $CRLF;
    print STDOUT $body;
}

sub query {
    my $name = shift;
    unless ($_GET) {
        my $input = $ENV{QUERY_STRING} || '';
        $_GET = _parse_query($input);
    }
    return $_GET->{$name} if !ref($_GET->{$name});
    return wantarray ? 
        @{$_GET->{$name}} : join(',',@{$_GET->{$name}});
}

sub post_param{
    my $name = shift || '';
    unless ($_POST) {
        my $input = '';
        if ( $ENV{REQUEST_METHOD} eq "POST" ) {
            if ( $ENV{CONTENT_LENGTH} > $MAX_POST_BODY_SIZE ) {
                die "too long Content-Length";
            }
            else {
                binmode STDIN;
                read( STDIN, $input, $ENV{CONTENT_LENGTH} );
            }
        }
        if ( my ($boundary) = $ENV{CONTENT_TYPE} =~ m!multipart/form-data;\s*boundary=(.*)$! ){
            $input =~ s/--$CRLF\Z/$CRLF/m;
            my $quote_b = quotemeta $boundary;
            my @inputs = split /--$quote_b$CRLF/m, $input;
            for ( @inputs ){
                my ( $key_data, $val ) = split /$CRLF$CRLF/m, $_, 2;
                next unless $val;
                $val =~ s/$CRLF\Z//m;
                my ($key) = $key_data =~ /^Content-Disposition: form-data; name="(.*?)"/;
                next unless $key;
                if ( my ($filename, $content_type ) = 
                        $key_data =~ /filename="(.*?)".*Content-Type: ([^\r\n]+)/ms 
                ){
                    my ( $ext ) = $filename =~ /\.([^.]+)$/;
                    $ext ||= '';
                    $_POST->{$key} = {
                        name => $filename,
                        ext  => $ext,
                        'Content-Type' => $content_type,
                        data => $val,
                    };
                }
                else{
                    $val = Encode::decode_utf8($val);
                    if ( $_POST->{$key} ){
                        if ( ref $_POST->{$key} eq 'ARRAY' ){
                            $_POST->{$key} = [ @{$_POST->{$key}}, $val];
                        }
                        else{
                            $_POST->{$key} = [$_POST->{$key}, $val];
                        }
                    }
                    else{
                        $_POST->{$key} = $val;
                    }
                }
            }
        }
        else{
            $_POST = _parse_query($input);
        }
    }
    return $_POST->{$name} if !(ref($_POST->{$name}) eq 'ARRAY');
    return wantarray ? 
        @{$_POST->{$name}} : join(',',@{$_POST->{$name}});
}

sub query_keys {
    query_param() unless $_GET;
    keys %$_GET;
}

sub post_keys {
    post_param() unless $_POST;
    keys %$_POST;
}

sub _parse_query{ #store list reference if existing multiple item
    my $input = shift;
    my %ret;
    for ( split /&/, $input ){
        my ( $key, $val ) = split /=/, $_;
        $val = url_decode($val) if $val;
        if( !$ret{$key} ){
            $ret{$key} = $val;
        }
        elsif( !ref($ret{$key}) ){
            $ret{$key} = [$ret{$key}, $val];
        }
        else{
            push @{$ret{$key}}, $val;
        }
    }
    return \%ret;
}


sub redirect {
    +{  headers => +{ Status => ( $_[1] || 303 ), Location => $_[0] },
        body => 'redirect to ' . $_[0],
    };
}

sub controller {
    my $name = shift;
    my $func = 'do_' . $name;
    die 'controller: '
        . $CURRENT_CLASS . '::'
        . $func
        . ' function is missing...'
        unless my $code = $CURRENT_CLASS->can($func);
    $code->(@_);
}

sub model {
    my $name = shift;
    my $func = 'model_' . $name;
    die 'model: ' . $CURRENT_CLASS . '::' . $func . ' function is missing...'
        unless my $code = $CURRENT_CLASS->can($func);
    $code->(@_);
}

sub view {
    my $name = shift;
    my $func = 'view_' . $name;
    if ( my $code = $CURRENT_CLASS->can($func) ){
        my $ret = $code->(@_);
        return $ret if ref($ret);
        return +{
            headers => +{},
            body    => $ret,
        };
    }
    else { #
        no strict 'refs';
        "$CURRENT_CLASS\::view_template"->( $name, @_ );
    }
}

#utils
sub url_encode {
    my $str = shift;
    utf8::encode($str) if utf8::is_utf8($str);
    $str =~ s/([^\w ])/'%'.unpack('H2', $1)/eg;
    $str =~ tr/ /+/;
    return $str;
}

sub url_decode {
    my $str = shift;
    $str =~ tr/+/ /;
    $str =~ s/%([0-9A-Fa-f][0-9A-Fa-f])/pack('H2', $1)/eg;
    $str = Encode::decode_utf8($str);
    return $str;
}

my %FILTERS = (
    html => sub {
        my $text = shift;
        $text =~ s/&/&amp;/g   if( $text );
        $text =~ s/</&lt;/g    if( $text );
        $text =~ s/>/&gt;/g    if( $text );
        $text =~ s/\"/&quot;/g if( $text );
        $text =~ s/'/&#39;/g   if( $text );
        $text;
    },
);

sub filter {
    return \%FILTERS if @_ == 0;
    my ( $text, @filters ) = @_;
    for my $filter (@filters) {
        next
            unless exists $FILTERS{$filter}
                && ref( $FILTERS{$filter} ) eq 'CODE';
        $text = $FILTERS{$filter}->($text);
    }
    $text;
}

sub docroot { 
    my $s = $ENV{SCRIPT_NAME} || '';
    $s =~ s|/[^/]+$||; 
    $s;
}

sub uri_for {
    my ( $path, $query ) = @_;
    my @q;
    while ( my ( $key, $val ) = each %$query ) {
        $val = join '', map {
            /^[a-zA-Z0-9_.!~*'()-]$/
                ? $_
                : '%'
                . uc( unpack( 'H2', $_ ) )
        } split //, $val;
        push @q, "${key}=${val}";
    }
    docroot . '/' . $path . ( scalar @q ? '?' . join( '&', @q ) : '' );
}

sub is_post_request {
    return $ENV{REQUEST_METHOD} eq "POST";
}

sub view_image {
    my $file = shift;
    my ($ext) = $file =~ /\.([^\.]+)$/;
    my $mime_type = $MEDIA_MIMETYPES->{$ext};
    my ( $data, $size ) = _read_binary($file);
    +{  headers => +{ 
            'Content-Type' => $mime_type,
            'Accept-Ranges' => 'bytes',
            'Content-Length' => $size,
        },
        body    => $data ,
    }
}

sub view_static {
    my $file = shift;
    my ($ext) = $file =~ /\.([^\.]+)$/;
    my $mime_type = $MEDIA_MIMETYPES->{$ext};
    my $data = read_file($file);
    +{  headers => +{ 
            'Content-Type' => $mime_type,
            'Content-Length' => length($data),
        },
        body    => $data ,
    }
}

sub render {
    my $file = shift;
    my ( undef, $code ) = template_builder($file);
    die 'template file not found (' . $file . ')' . $code unless $code;
    my $result = ( eval $code )->(@_);
    $result = Encode::decode_utf8($result) unless Encode::is_utf8($result);
    $result;
}

sub view_template {
    my $file = shift;
    my ( $mime_type, $code ) = template_builder($file);
    die 'template file not found (' . $file . ')' . $code unless $code;
    +{  headers => +{ 'Content-Type' => $mime_type },
        body    => ( eval $code )->(@_),
    };
}

sub template_builder {
    my $file = shift;
    $file
        = $TEMPLATE_PARAMS->{prefix} . $file . '.' . $TEMPLATE_PARAMS->{ext};
    open my $fh, '<:utf8', $file or die $! . ': ' . $file;

    # sub parse {
    my $tmpl = do { local $/; $TEMPLATE_PARAMS->{line_start}.' my %stash = @_' . "\n" . <$fh> };
    my ($ext) = $file =~ /\.([^\.]+)$/;
    my $mime_type = $TEMPLATE_MIMETYPES->{$ext}
        || $TEMPLATE_MIMETYPES->{default};

    # Clean start
    my @tree;

    # Tags
    my $line_start    = quotemeta $TEMPLATE_PARAMS->{line_start};
    my $tag_start     = quotemeta $TEMPLATE_PARAMS->{tag_start};
    my $tag_end       = quotemeta $TEMPLATE_PARAMS->{tag_end};
    my $cmnt_mark     = quotemeta $TEMPLATE_PARAMS->{comment_mark};
    my $expr_mark     = quotemeta $TEMPLATE_PARAMS->{expression_mark};
    my $raw_expr_mark = quotemeta $TEMPLATE_PARAMS->{raw_expression_mark};

    # Tokenize
    my $state                = 'text';
    my $multiline_expression = 0;
    for my $line ( split /\n/, $tmpl ) {

        # Perl line without return value
        if ( $line =~ /^$line_start\s+(.+)$/ ) {
            push @tree, [ 'code', $1 ];
            $multiline_expression = 0;
            next;
        }

        # Perl line with return value
        if ( $line =~ /^$line_start$expr_mark\s+(.+)$/ ) {
            push @tree, [ 'expr', $1 ];
            $multiline_expression = 0;
            next;
        }

        # Perl line with raw return value
        if ( $line =~ /^$line_start$raw_expr_mark\s+(.+)$/ ) {
            push @tree, [ 'raw_expr', $1 ];
            $multiline_expression = 0;
            next;
        }

        # Comment line, dummy token needed for line count
        if ( $line =~ /^$line_start$cmnt_mark\s+(.+)$/ ) {
            push @tree, [];
            $multiline_expression = 0;
            next;
        }

        # Escaped line ending?
        if ( $line =~ /(\\+)$/ ) {
            my $length = length $1;

            # Newline escaped
            if ( $length == 1 ) {
                $line =~ s/\\$//;
            }

            # Backslash escaped
            if ( $length >= 2 ) {
                $line =~ s/\\\\$/\\/;
                $line .= "\n";
            }
        }

        # Normal line ending
        else { $line .= "\n" }

        # Mixed line
        my @token;
        for my $token (
            split /
            (
                $tag_start$raw_expr_mark # Raw Expression
            |
                $tag_start$expr_mark   # Expression
            |
                $tag_start$cmnt_mark   # Comment
            |
                $tag_start             # Code
            |
                $tag_end               # End
            )
        /x, $line
            )
        {

            # Garbage
            next unless $token;

            # End
            if ( $token =~ /^$tag_end$/ ) {
                $state                = 'text';
                $multiline_expression = 0;
            }

            # Code
            elsif ( $token =~ /^$tag_start$/ ) { $state = 'code' }

            # Comment
            elsif ( $token =~ /^$tag_start$cmnt_mark$/ ) { $state = 'cmnt' }

            # Raw Expression
            elsif ( $token =~ /^$tag_start$raw_expr_mark$/ ) {
                $state = 'raw_expr';
            }

            # Expression
            elsif ( $token =~ /^$tag_start$expr_mark$/ ) {
                $state = 'expr';
            }

            # Value
            else {

                # Comments are ignored
                next if $state eq 'cmnt';

                # Multiline expressions are a bit complicated,
                # only the first line can be compiled as 'expr'
                $state = 'code' if $multiline_expression;
                $multiline_expression = 1 if $state eq 'expr';

                # Store value
                push @token, $state, $token;
            }
        }
        push @tree, \@token;
    }

    # }
    # sub build {

    # Compile
    my @lines;
    for my $line (@tree) {

        # New line
        push @lines, '';
        for ( my $j = 0; $j < @{$line}; $j += 2 ) {
            my $type  = $line->[$j];
            my $value = $line->[ $j + 1 ];

            # Need to fix line ending?
            my $newline = chomp $value;

            # Text
            if ( $type eq 'text' ) {

                # Quote and fix line ending
                $value = quotemeta($value);
                $value .= '\n' if $newline;

                $lines[-1] .= "\$_MOJO .= \"" . $value . "\";";
            }

            # Code
            if ( $type eq 'code' ) {
                $lines[-1] .= "$value;";
            }

            # Expression
            if ( $type eq 'expr' ) {
                $lines[-1]
                    .= "\$_MOJO .= filter( scalar($value) => 'html' );";
            }

            # Raw Expression
            if ( $type eq 'raw_expr' ) {
                $lines[-1] .= "\$_MOJO .= $value;";
            }
        }
    }

    # Wrap
    $lines[0] ||= '';
    $lines[0] = q/sub { my $_MOJO = '';/ . $lines[0];
    $lines[-1] .= q/return $_MOJO; };/;

    return $mime_type => join( "\n", @lines );

    # }
}
### EXTEND template END

sub run(&){
    $_[0];
}

sub _get_ready_controller {
    my $action = shift;
    local $1;
    for my $reg ( keys %DISPATCH_RULES ){
        if ( my @snippets = $action =~ $reg ){
            @snippets = () unless defined $1;
            return ( $DISPATCH_RULES{$reg}, @snippets );
        }
    }
    $action;
}

sub dispatch_rules {
    %DISPATCH_RULES = (%DISPATCH_RULES, @_);
}

sub logging{
    my $msg = shift;
    my ($package, $filename, $line) = caller;
    my ($sec, $min, $hour, $day, $mon, $year, undef, undef, undef) 
        = localtime;
    $mon++;$year+=1900;
    open my $fh,'>>:utf8',$HyCycjk::LOG_PATH.'app'.sprintf("%04d%02d%02d",$year,$mon,$day).'.log' or return;
    print $fh
        sprintf("%04d/%02d/%02d %02d:%02d:%02d ",$year,$mon,$day,$hour,$min,$sec).
        "$filename($line) ".
        $msg."\n";
}

sub app_config {
    return $HyCycjk::APP_CONFIG->{shift};
}

{ no strict 'refs';
    for my $meth (qw/get set keys remove as_hashref expire regenerate_session_id session_id/) {
        *{ __PACKAGE__ . '::'. "session_$meth"} = sub {
            $_SESSION->$meth(@_);
        };
    }
}

package HyCycjk::Session;
use Fcntl;
use Storable;

our $COOKIE_CLASS = 'CGI::Cookie';
{
    no strict 'refs';
    # ro_accessors
    for my $meth ( qw/sid_length permissive name path domain expires file secure/ ){
        *{__PACKAGE__."::$meth"} = sub {
            shift->{$meth};
        }
    }
    # rw_accessors
    for my $meth ( qw/session_id _data is_changed is_fresh/ ){
        *{__PACKAGE__."::$meth"} = sub {
            my $self = shift;
            $self->{$meth} = shift if @_;
            $self->{$meth};
        }
    }
}

sub new {
    my $class = shift;
    my %args = ref($_[0]) ? %{$_[0]} : @_;

    # set default values
    $args{_data}      ||= {};
    $args{is_changed} ||= 0;
    $args{is_fresh}   ||= 0;
    $args{sid_length} ||= 32;
    $args{file}       ||= $HyCycjk::WORK_PATH.'session';

    $args{permissive} ||= 0;

    $args{name}       ||= 'http_session_id';
    $args{path}       ||= '/';
    $args{domain}     ||= '';
    $args{expires}    ||= '';
    $args{secure}     ||= '';

    my $self = bless {%args}, $class;
    $self->_load_session();

    $self;
}

sub _load_session {
    my $self = shift;

    my $session_id = $self->get_session_id;
    if ( $session_id ) {
        my $data = $self->select($session_id);
        if ($data) {
            $self->session_id( $session_id );
            $self->_data($data);
        } else {
            if ($self->permissive) {
                $self->session_id( $session_id );
                $self->is_fresh(1);
            } else {
                # session was expired? or session fixation?
                # regen session id.
                $self->session_id( $self->_generate_session_id() );
                $self->is_fresh(1);
            }
        }
    } else {
        # no sid; generate it
        $self->session_id( $self->_generate_session_id() );
        $self->is_fresh(1);
    }
}

sub _generate_session_id {
    my $self = shift;
    $self->generate_id($self->sid_length);
}

sub dbm {
    my $self = shift;
    $self->{dbm} ||= do {
        my %hash;
        eval "use SDBM_File";
        tie %hash, 'SDBM_File', $self->file, O_CREAT | O_RDWR, oct("600");
        \%hash;
    };
}

sub select {
    my ( $self, $key ) = @_;
    Storable::thaw $self->dbm->{$key};
}

sub update {
    my ( $self, $key, $value ) = @_;
    $self->dbm->{$key} = Storable::freeze $value;
}

sub delete {
    my ( $self, $key ) = @_;
    delete $self->dbm->{$key};
}

sub cleanup { Carp::croak "This storage doesn't support cleanup" }

sub finalize {
    my ($self, ) = @_;
    if ($self->is_fresh || $self->is_changed) {
        $self->update( $self->session_id, $self->_data );
    }
    delete $self->{$_} for keys %$self;
    bless $self, 'HyCycjk::Session::Finalized';
}
{
    my $required = 0;
    sub _cookie_class {
        my $class = shift;
        unless ($required) {
            (my $klass = $COOKIE_CLASS) =~ s!::!/!g;
            $klass .= ".pm";
            require $klass;
            $required++;
        }
        return $COOKIE_CLASS
    }
}

sub cookie_str {
    my $self = shift;
    my $cookie = _cookie_class()->new(
        sub {
            my %options = (
                -name   => $self->name,
                -value  => $self->session_id,
                -path   => $self->path,
            );
            $options{'-domain'} = $self->domain if $self->domain;
            $options{'-expires'} = $self->expires if $self->expires;
            $options{'-secure'} = $self->secure if $self->secure;
            %options;
        }->()
    );
    return $cookie->as_string;
}

sub get_session_id {
    my ($self) = @_;
    my $cookie_header = $ENV{HTTP_COOKIE};
    return unless $cookie_header;

    my %jar    = _cookie_class()->parse($cookie_header);
    my $cookie = $jar{$self->name};
    return $cookie ? $cookie->value : undef;
}

{
    my $digest_func = '';
    sub generate_id {
        unless ( $digest_func ) {
            no strict 'refs';
            require Time::HiRes;
            eval {
                require Digest::SHA;
                $digest_func = *{Digest::SHA::sha256_hex};
            };
            if( $@ ){
                $@ = undef;
                require Digest::MD5;
                $digest_func = *{Digest::MD5::md5_hex};
            }
        }
        my ($class, $sid_length) = @_;
        my $unique = $ENV{UNIQUE_ID} || ( [] . rand() );
        return substr( $digest_func->( Time::HiRes::gettimeofday() . $unique ), 0, $sid_length );
    }
}
sub keys {
    my $self = shift;
    return keys %{ $self->_data };
}

sub get {
    my ($self, $key) = @_;
    $self->_data->{$key};
}

sub set {
    my ($self, $key, $val) = @_;
    $self->is_changed(1);
    $self->_data->{$key} = $val;
}

sub remove {
    my ( $self, $key ) = @_;
    $self->is_changed(1);
    delete $self->_data->{$key};
}

sub as_hashref {
    my $self = shift;
    return { %{ $self->_data } }; # shallow copy
}

sub expire {
    my $self = shift;
    $self->delete($self->session_id);

    # XXX tricky bit to unlock
    delete $self->{$_} for qw(is_fresh is_changed);
    $self->finalize;

    # rebless to null class
    bless $self, 'HyCycjk::Session::Expired';
}

sub regenerate_session_id {
    my ($self, $delete_old) = @_;
    $self->_data( { %{ $self->_data } } );

    if ($delete_old) {
        my $oldsid = $self->session_id;
        $self->delete($oldsid);
    }
    my $session_id = $self->_generate_session_id();
    $self->session_id( $session_id );
    $self->is_fresh(1);
}

package HyCycjk::Session::Finalized;
sub is_fresh { 0 }
sub AUTOLOAD { }

package HyCycjk::Session::Expired;
sub is_fresh { 0 }
sub AUTOLOAD { }



1;
__END__

=head1 NAME

HyCycjk - Haiyou yige CGI yingyong chengxu jiemian kuangjia
--还有一个CGI应用程序界面框架

=head1 SYNOPSIS

  use HyCycjk;

=head1 DESCRIPTION

HyCycjk is Light Weight Web Application Framework based on Yacafi.

=head1 AUTHOR

Masayuki Matsuki E<lt>y.songmu <at> gmail <döt> comE<gt>

=head1 SEE ALSO

=head1 REPOSITORY


=head1 LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

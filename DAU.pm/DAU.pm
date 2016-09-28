################################################################################
# $Id: DAU.pm 288 2009-06-01 14:00:17Z heidinger $
################################################################################
#
# DAU.pm - write like an idiot
#
################################################################################
# Description
################################################################################
#
# dau.pl is a Perl script for the IRC client irssi.
# Due to its popularity I decided to write and maintain this module to have a
# common basis for all the other programs which want to use a function that
# dau.pl for irssi provides.
#
################################################################################
# Author
################################################################################
#
# Clemens Heidinger <heidinger@dau.pl>
#
################################################################################
# Credits
################################################################################
#
# - Robert Hennig: For the original dau shell script. Out of this script,
#   merged with some other small Perl and shell scripts and aliases arised the
#   first version of dau.pl for irssi.
#
################################################################################
# Documentation
################################################################################
#
# The @INC array contains a list of directories to consult when 'use' pulls in
# code from another file. Therefore you have to store DAU.pm in one of these
# directories.
# After that and an 'use DAU;' you can access the following function:
#
# parse_text("--stutter --moron test test test");
#
# returns the same string as
#
# /dau --stutter --moron test test test
#
# in dau.pl for irssi.
#
# If you need more information about these --switches look in the documentation
# of dau.pl for irssi.
#
# There are a couple of variables controlling the behaviour of the switches.
# Check the section "Configuration: Global variables to be exported" down the
# file.
# They do the same as the corresponding settings in dau.pl for irssi.
# Therefore look in the documentation of dau.pl for irssi if you need further
# explanations.
#
################################################################################
# License
################################################################################
#
# Licensed under the BSD license.
#
################################################################################
# Website
################################################################################
#
# http://dau.pl/
#
# For additional information, DAU.pm, and the dauomat.
#
################################################################################

package DAU;

use 5.6.0;
use Exporter;
use File::Basename;
use File::Path;
use IPC::Open3;
use locale;
use POSIX;
use re 'eval';
use strict;
use vars qw(%option);

our (@ISA, @EXPORT, $VERSION);

$VERSION = '2.5.0';
#$VERSION = '2.5.0 SVN ($LastChangedRevision: 288 $)';
@EXPORT = qw(%option &parse_text);
@ISA = qw(Exporter);

################################################################################
# Configuration: Global variables to be exported
################################################################################

# All the options

$option{dau_cool_eol_style}                = 'random';
$option{dau_cool_maximum_line}             = 2;
$option{dau_cool_probability_eol}          = 20;
$option{dau_cool_probability_word}         = 20;
$option{dau_cowsay_cowlist}                = '';
$option{dau_cowsay_cowpath}                = &def_dau_cowsay_cowpath;
$option{dau_cowsay_cowpolicy}              = 'allow';
$option{dau_cowsay_cowsay_path}            = &def_dau_cowsay_cowsay_path;
$option{dau_cowsay_cowthink_path}          = &def_dau_cowsay_cowthink_path;
$option{dau_delimiter_string}              = ' ';
$option{dau_figlet_fontlist}               = 'mnemonic,term,ivrit';
$option{dau_figlet_fontpath}               = &def_dau_figlet_fontpath;
$option{dau_figlet_fontpolicy}             = 'allow';
$option{dau_figlet_path}                   = &def_dau_figlet_path;
$option{dau_files_cool_suffixes}           = 'cool_suffixes';
$option{dau_files_root_directory}          = "$ENV{HOME}/.dau";
$option{dau_files_substitute}              = 'substitute.pl';
$option{dau_language}                      = 'en';
$option{dau_moron_eol_style}               = 'random';
$option{dau_random_options}                =
                                             '--substitute --boxes --uppercase,' .
                                             '--substitute --cool,' .
                                             '--substitute --delimiter,' .
                                             '--substitute --dots --moron,' .
                                             '--substitute --leet,' .
                                             '--substitute --mix,' .
                                             '--substitute --mixedcase --bracket,' .
                                             '--substitute --moron --stutter --uppercase,' .
                                             '--substitute --moron -omega on,' .
                                             '--substitute --moron,' x 5 .
                                             '--substitute --words --mixedcase'
;
$option{dau_silence}                       = 1;
$option{dau_standard_messages}             = '';
$option{dau_standard_options}              = '--random';
$option{dau_words_range}                   = '1-4';

################################################################################
# Global variables NOT to be exported
################################################################################

# The command to use for the output (MSG f.e.)

our $command_out;

# '--command -out' used?

our $command_out_activated;

# Counter for the subroutines entered

our $counter_subroutines;

# Counter for the switches
# --me --moron: --me would be 0, --moron 1

our $counter_switches;

# daumode activated?

our $daumode_activated;

# print() the message or not?

our $print_message;

# Queue holding the switches

our %queue;

# Remember the last switches used by --random so that they don't repeat

our $random_last;

# All switches that may be given at commandline

our %switches = (

    # These switches may be combined

    combo  => {
                boxes     => { 'sub'  => \&switch_boxes },
                bracket   => {
                              'sub' => \&switch_bracket,
                               left  => { '*' => 1 },
                               right => { '*' => 1 },
                             },
                chars     => { 'sub' => \&switch_chars },
                color     => {
                              'sub'   => \&switch_color,
                              codes   => { '*' => 1 },
                              random  => {
                                           off => 1,
                                           on  => 1,
                                          },
                              'split' => {
                                          capitals  => 1,
                                          chars     => 1,
                                          lines     => 1,
                                          paragraph => 1,
                                          rchars    => 1,
                                          words     => 1,
                                         },
                             },
                command   => {
                              'sub' => \&switch_command,
                               in   => { '*' => 1 },
                               out  => { '*' => 1 },
                               },
                cool      => {
                              'sub'      => \&switch_cool,
                               eol_style => {
                                             suffixes          => 1,
                                             exclamation_marks => 1,
                                             random            => 1,
                                            },
                               max       => { '*' => 1 },
                               prob_eol  => { '*' => 1 },
                               prob_word => { '*' => 1 },
                             },
                cowsay    => {
                              'sub'       => \&switch_cowsay,
                               arguments  => { '*' => 1 },
                               think      => {
                                              off => 1,
                                              on  => 1,
                                             },
                             },
                delimiter => {
                              'sub'    => \&switch_delimiter,
                               string  => { '*' => 1 },
                             },
                dots      => { 'sub' => \&switch_dots },
                figlet    => { 'sub' => \&switch_figlet },
                me        => { 'sub' => \&switch_me },
                mix       => { 'sub' => \&switch_mix },
                moron     => {
                              'sub'      => \&switch_moron,
                               eol_style => {
                                             nothing => 1,
                                             random  => 1,
                                            },
                               level     => { '*' => 1 },
                               omega     => {
                                             off => 1,
                                             on  => 1,
                                            },
                               typo      => {
                                             off => 1,
                                             on  => 1,
                                            },
                               uppercase => {
                                             off => 1,
                                             on  => 1,
                                            },
                             },
                leet          => { 'sub' => \&switch_leet },
                mixedcase     => { 'sub' => \&switch_mixedcase },
                nothing       => { 'sub' => \&switch_nothing },
                parse_special => {
                                  'sub' => \&switch_parse_special,
                                  irssi_variables => {
                                                      off => 1,
                                                      on  => 1,
                                                     },
                                  list_delimiter  => { '*' => 1 },
                                 },
                'reverse'     => { 'sub' => \&switch_reverse },
                stutter       => { 'sub' => \&switch_stutter },
                substitute    => {
                                  'sub' => \&switch_substitute,
                                  file  => { '*' => 1 },
                                 },
                underline     => { 'sub' => \&switch_underline },
                uppercase     => { 'sub' => \&switch_uppercase },
                words         => { 'sub' => \&switch_words },
               },

    # The following switches must not be combined

    nocombo => {
                away         => {
                                 'sub' => \&switch_away,
                                 channels => { '*' => 1 },
                                 interval => { '*' => 1 },
                                 reminder => {
                                              on  => 1,
                                              off => 1,
                                             },
                                },
                babble       => {
                                 'sub'        => \&switch_babble,
                                 at           => { '*' => 1 },
                                 cancel       => {
                                                  on  => 1,
                                                  off => 1,
                                                 },
                                 filter       => { '*' => 1 },
                                 history_size => { '*' => 1 },
                                },
                changelog    => { 'sub' => \&switch_changelog },
                create_files => { 'sub' => \&switch_create_files },
                daumode      => {
                                 'sub'      => \&switch_daumode,
                                  modes_in  => { '*' => 1 },
                                  modes_out => { '*' => 1 },
                                  perm      => {
                                                '00' => 1,
                                                '01' => 1,
                                                '10' => 1,
                                                '11' => 1,
                                               },
                                },
                help         => {
                                 'sub'     => \&switch_help,

                                 # setting changed/added => change/add it here

                                 setting => {
                                             # boolean
                                             dau_away_quote_reason               => 1,
                                             dau_away_reminder                   => 1,
                                             dau_babble_verbose                  => 1,
                                             dau_color_choose_colors_randomly    => 1,
                                             dau_cowsay_print_cow                => 1,
                                             dau_figlet_print_font               => 1,
                                             dau_silence                         => 1,
                                             dau_statusbar_daumode_hide_when_off => 1,
                                             dau_tab_completion                  => 1,

                                             # Integer
                                             dau_babble_history_size             => 1,
                                             dau_babble_verbose_minimum_lines    => 1,
                                             dau_cool_maximum_line               => 1,
                                             dau_cool_probability_eol            => 1,
                                             dau_cool_probability_word           => 1,
                                             dau_remote_babble_interval_accuracy => 1,

                                             # String
                                             dau_away_away_text                  => 1,
                                             dau_away_back_text                  => 1,
                                             dau_away_options                    => 1,
                                             dau_away_reminder_interval          => 1,
                                             dau_away_reminder_text              => 1,
                                             dau_babble_options_line_by_line     => 1,
                                             dau_babble_options_preprocessing    => 1,
                                             dau_color_codes                     => 1,
                                             dau_cool_eol_style                  => 1,
                                             dau_cowsay_cowlist                  => 1,
                                             dau_cowsay_cowpath                  => 1,
                                             dau_cowsay_cowpolicy                => 1,
                                             dau_cowsay_cowsay_path              => 1,
                                             dau_cowsay_cowthink_path            => 1,
                                             dau_daumode_channels                => 1,
                                             dau_delimiter_string                => 1,
                                             dau_figlet_fontlist                 => 1,
                                             dau_figlet_fontpath                 => 1,
                                             dau_figlet_fontpolicy               => 1,
                                             dau_figlet_path                     => 1,
                                             dau_files_away                      => 1,
                                             dau_files_babble_messages           => 1,
                                             dau_files_cool_suffixes             => 1,
                                             dau_files_root_directory            => 1,
                                             dau_files_substitute                => 1,
                                             dau_language                        => 1,
                                             dau_moron_eol_style                 => 1,
                                             dau_parse_special_list_delimiter    => 1,
                                             dau_random_options                  => 1,
                                             dau_remote_babble_channellist       => 1,
                                             dau_remote_babble_channelpolicy     => 1,
                                             dau_remote_babble_interval          => 1,
                                             dau_remote_channellist              => 1,
                                             dau_remote_channelpolicy            => 1,
                                             dau_remote_deop_reply               => 1,
                                             dau_remote_devoice_reply            => 1,
                                             dau_remote_op_reply                 => 1,
                                             dau_remote_permissions              => 1,
                                             dau_remote_question_regexp          => 1,
                                             dau_remote_question_reply           => 1,
                                             dau_remote_voice_reply              => 1,
                                             dau_standard_messages               => 1,
                                             dau_standard_options                => 1,
                                             dau_words_range                     => 1,
                                            },
                                },
                long_help => { 'sub'    => \&switch_long_help },
                random    => { 'sub'    => \&switch_random,
                                verbose => {
                                            off => 1,
                                            on  => 1,
                                           },
                             },
               },
);

################################################################################
# Code run once at start
################################################################################

cowsay_cowlist($option{dau_cowsay_cowpath});
figlet_fontlist($option{dau_figlet_fontpath});

################################################################################
# Subroutines (switches, must not be combined)
################################################################################

sub switch_away {
	my ($data, $channel) = @_;

	return $data;
}

sub switch_babble {
	my ($data, $channel) = @_;

	return $data;
}

sub switch_changelog {
	my ($data, $channel) = @_;

	return $data;
}

sub switch_create_files {

	# create directory dau_files_root_directory if not found

	if (-f $option{dau_files_root_directory}) {
		print_err("$option{dau_files_root_directory} is a _file_ => aborting");
		return;
	}
	if (-d $option{dau_files_root_directory}) {
		print_out('directory dau_files_root_directory already exists - no need to create it');
	} else {
		if (mkpath([$option{dau_files_root_directory}])) {
			print_out("creating directory $option{dau_files_root_directory}/");
		} else {
			print_err("failed creating directory $option{dau_files_root_directory}/");
		}
	}

	# create file dau_files_substitute if not found

	my $file1 = "$option{dau_files_root_directory}/$option{dau_files_substitute}";

	if (-e $file1) {

		print_out("file $file1 already exists - no need to create it");

	} else {

		if (open(FH1, "> $file1")) {

			print FH1 &fix(<<'			END');
			# dau.pl - http://dau.pl/
			#
			# This is the file --moron will use for your own substitutions.
			# You can use any perlcode in here.
			# $_ contains the text you can work with.
			# $_ has to contain the data to be returned to dau.pl at the end.
			END

			print_out("$file1 created. you should edit it now!");

		} else {

			print_err("cannot write $file1: $!");

		}

		if (!close(FH1)) {
			print_err("cannot close $file1: $!");
		}
	}

	# create file dau_files_babble_messages if not found

	my $file2 = "$option{dau_files_root_directory}/$option{dau_files_babble_messages}";

	if (-e $file2) {

		print_out("file $file2 already exists - no need to create it");

	} else {

		if (open(FH1, "> $file2")) {

			print FH1 &fix(<<'			END');
			END

			print_out("$file2 created. you should edit it now!");

		} else {

			print_err("cannot write $file2: $!");

		}

		if (!close(FH1)) {
			print_err("cannot close $file2: $!");
		}
	}

	# create file dau_files_cool_suffixes if not found

	my $file3 = "$option{dau_files_root_directory}/$option{dau_files_cool_suffixes}";

	if (-e $file3) {

		print_out("file $file3 already exists - no need to create it");

	} else {

		if (open(FH1, "> $file3")) {

			print FH1 &fix(<<'			END');
			END

			print_out("$file3 created. you should edit it now!");

		} else {

			print_err("cannot write $file3: $!");

		}

		if (!close(FH1)) {
			print_err("cannot close $file3: $!");
		}
	}

	return;
}

sub switch_daumode {
	my ($data, $channel) = @_;

	return $data;
}

sub switch_help {
	my ($data, $channel) = @_;

	return $data;
}

sub switch_long_help {
	my ($data, $channel) = @_;

	return $data;
}

sub switch_random {
	my ($data, $channel_rec) = @_;
	my $output;
	my (@options, $opt, $text);

	# Push each item of dau_random_options in the @options array.

	while ($option{dau_random_options} =~ /\s*([^,]+)\s*,?/g) {
		my $item = $1;
		push @options, $item;
	}

	# More than one item in @options. Choose one randomly but exclude
	# the last item chosen.

	if (@options > 1) {
		@options = grep { $_ ne $random_last } @options;
		$opt = @options[rand(@options)];
		$random_last = $opt;
	}

	# Exact one item in @options - take that

	elsif (@options == 1) {
		$opt = $options[0];
		$random_last = $opt;
	}


	# No item in @options - call switch_moron()

	else {
		$opt = '--moron';
	}

	# dauify it!

	unless (lc(return_option('random', 'verbose')) eq 'off') {
		print_out("%9--random%9 has chosen %9$opt%9", $channel_rec);
	}
	$text .= $opt . ' ' . $data;
	$output = parse_text($text, $channel_rec);

	return $output;
}

################################################################################
# Subroutines (switches, may be combined)
################################################################################

sub switch_boxes {
	my $data = shift;

	# handling punctuation marks:
	# they will be put in their own box later

	$data =~ s%(\w+)([,.?!;:]+)%
	           $1 . ' ' . join(' ', split(//, $2))
	          %egx;

	# separate words (by whitespace) and put them in a box

	$data =~ s/(\s*)(\S+)(\s*)/$1\[$2\]$3/g;

	return $data;
}

sub switch_bracket {
	my $data = shift;
	my $output;

	my $option_left  = return_option('bracket', 'left');
	my $option_right = return_option('bracket', 'right');

	my %brackets = (
                        '(('   => '))',
                        '-=('  => ')=-',
                        '-=['  => ']=-',
                        '-={'  => '}=-',
                        '-=|(' => ')|=-',
                        '-=|[' => ']|=-',
                        '-=|{' => '}|=-',
                        '.:>'  => '<:.',
                       );

	foreach (keys %brackets) {
		for my $times (2 .. 3) {
			my $pre  = $_;
			my $post = $brackets{$_};
			$pre  =~ s/(.)/$1 x $times/eg;
			$post =~ s/(.)/$1 x $times/eg;

			$brackets{$pre} = $post;
		}
	}

	$brackets{'!---?['} = ']?---!';
	$brackets{'(qp=>'}  = '<=qp)';
	$brackets{'----->'} = '<-----';

	my ($left, $right);
	if ($option_left && $option_right) {
		$left  = $option_left;
		$right = $option_right;
	} else {
		$left  = (keys(%brackets))[int(rand(keys(%brackets)))];
		$right = $brackets{$left};
	}

	$output = "$left $data $right";

	return $output;
}

sub switch_chars {
	my $data = shift;
	my $output;

	foreach my $char (split //, $data) {
		$output .= "$char\n";
	}
	return $output;
}

sub switch_command {
	my ($data, $channel) = @_;

	return $data;
}

sub switch_color {
	my ($data, $channel) = @_;

	return $data;
}

sub switch_cool {
	my ($data, $channel) = @_;
	my $output;

	################################################################################
	# Get the options
	################################################################################

	my $option_eol_style = return_option('cool', 'eol_style', $option{dau_cool_eol_style});

	my $option_max = return_option('cool', 'max', $option{dau_cool_maximum_line});
	if (!defined($option_max) || int($option_max) < 0) {
		$option_max = INT_MAX;
	}

	my $option_prob_eol = return_option('cool', 'prob_eol', $option{dau_cool_probability_eol});
	if (!defined($option_prob_eol) || int($option_prob_eol) < 0 || int($option_prob_eol) > 100) {
		$option_prob_eol = 20;
	}

	my $option_prob_word = return_option('cool', 'prob_word', $option{dau_cool_probability_word});
	if (!defined($option_prob_word) || int($option_prob_word) < 0 || int($option_prob_word) > 100) {
		$option_prob_word = 20;
	}

	################################################################################
	# Insert the trademarks and dollar signs
	################################################################################

	my $max = $option_max;
	foreach my $line (split /(\n)/, $data) {
		foreach my $word (split /(\s)/, $line) {
			if ($max > 0 && (rand(100) <= $option_prob_word) && $word =~ /^(\w+)([[:punct:]])?$/) {
				$word = "${1}[tm]${2}";
				$max--;
			}
			if ($max > 0 && (rand(100) <= $option_prob_word) && $word =~ /^(\w+(?:\[tm\])?)([[:punct:]])?$/) {
				$word = "\$${1}${2}";
				$max--;
			}
			$output .= $word;
		}
		$max = $option_max;
	}

	################################################################################
	# Reversed smileys
	################################################################################

	my $hat = '[(<]';
	my $eyes = '[:;%]';
	my $nose = '[-]';
	my $mouth = '[)(><\[\]{}|]';

	$output =~ s{($hat?$eyes$nose?$mouth+)}{
	             # Supposed to be read from the right to the left.
	             # Therefore reverse all parenthesis characters:

	             my $tr = $1;
	             $tr =~ tr/()<>[]\{\}/)(><][\}\{/;

	             # Reverse the rest

	             reverse($tr);
	            }egox;

	################################################################################
	# EOL modifications
	################################################################################

	my $style = $option_eol_style;
	if ($option_eol_style eq 'random') {
		if (int(rand(2)) && $output !~ /[?!]$/) {
			$style = 'exclamation_marks';
		} else {
			$style = 'suffixes';
		}
	}

	# If there is no suffixes file, go for the exclamation marks

	my $file = "$option{dau_files_root_directory}/$option{dau_files_cool_suffixes}";
	unless (-e $file && -r $file && !(-z $file)) {
		$style = 'exclamation_marks';
	}

	# Skip EOL modifications?

	if (int(rand(100)) > $option_prob_eol) {
		$style = 'none';
	}

	# Style determined. Act accordingly:

	if ($style eq 'exclamation_marks') {
		my @eol;
		if ($option{dau_language} eq 'de') {
			@eol = ("eins", "shifteins", "elf", "hundertelf", "tausendeinhundertundelf");
			for (1 .. 5) {
				push(@eol, "eins");
				push(@eol, "elf");
			}
		} else {
			@eol = ("one", "shiftone", "eleven");
			for (1 .. 5) {
				push(@eol, "one");
				push(@eol, "eleven");
			}
		}

		$output =~ s/\s*([,.?!])*\s*$//;
		$output .= '!' x (3 + int(rand(3)));
		$output .= '1' x (3 + int(rand(3)));
		$output .= $eol[rand(@eol)] x (1 + int(rand(1)));
		$output .= $eol[rand(@eol)] x (int(rand(2)));
	} elsif ($style eq 'suffixes') {
		my $suffix;
		if (-e $file && -r $file) {
			$/ = "\n";
			@ARGV = ($file);
			srand;
			rand($.) < 1 && ($suffix = switch_parse_special($_, $channel)) while <>;
		}
		$output =~ s/\s*$//;

		if ($output =~ /^\s*$/) {
			$output = $suffix;
		} else {
			$output .= " " . $suffix;
		}
	}

	return $output;
}

sub switch_cowsay {
	my $data = shift;
	my ($binarypath, $output, @cows, %cow, $cow, @cache1, @cache2);
	my $skip = 1;
	my $think = return_option('cowsay', 'think');

	my $executable_name;
	if ($think eq 'on') {
		$binarypath = $option{dau_cowsay_cowthink_path};
		$executable_name = 'cowthink';
	} else {
		$binarypath = $option{dau_cowsay_cowsay_path};
		$executable_name = 'cowsay';
	}

	if (-e $binarypath && !(-f $binarypath)) {
		print_err("dau_cowsay_${executable_name}_path has to point to the $executable_name executable.");
		return;
	} elsif (!(-e $binarypath)) {
		print_err("$executable_name not found. Install it and set dau_cowsay_${executable_name}_path.");
		return;
	}

	if (return_option('cowsay', 'cow')) {
		$cow = return_option('cowsay', 'cow');
	} else {
		while ($option{dau_cowsay_cowlist} =~ /\s*([^,\s]+)\s*,?/g) {
			$cow{$1} = 1;
		}
		foreach my $cow (keys %{ $switches{combo}{cowsay}{cow} }) {
			if (lc($option{dau_cowsay_cowpolicy}) eq 'allow') {
				push(@cows, $cow)
					unless ($cow{$cow});
			} elsif (lc($option{dau_cowsay_cowpolicy}) eq 'deny') {
				push(@cows, $cow)
					if ($cow{$cow});
			} else {
				print_err('Invalid value for dau_cowsay_cowpolicy');
				return;
			}
		}
		if (@cows == 0) {
			print_err('Cannot find any cowsay cow.');
			return;
		}
		$cow = $cows[rand(@cows)];
	}

	# Run cowsay or cowthink

	local(*HIS_IN, *HIS_OUT, *HIS_ERR);
	my @arguments;
	my $option_arguments = return_option('cowsay', 'arguments');
	if ($option_arguments) {
		@arguments = split(/ /, $option_arguments);
	}
	my $childpid = open3(*HIS_IN, *HIS_OUT, *HIS_ERR, $binarypath, '-f', $cow, @arguments);

	print HIS_IN $data or return;
	close(HIS_IN) or return;

	my @errlines = <HIS_ERR>;
	my @outlines = <HIS_OUT>;
	close(HIS_ERR) or return;
	close(HIS_OUT) or return;

	waitpid($childpid, 0);
	if ($?) {
		print_err("That child exited with wait status of $?");
	}

	# Error during execution? Print errors and return

	unless (@errlines == 0) {
		print_err('Error during execution of cowsay');
		foreach my $line (@errlines) {
			print_err($line);
		}
		return;
	}

	if ($option{dau_cowsay_print_cow}) {
		print_out("Using cowsay cow $cow");
	}

	foreach (@outlines) {
		chomp;
		if (/^\s*$/ && $skip) {
			next;
		} else {
			$skip = 0;
		}
		push(@cache1, $_);
	}
	$skip = 1;
	foreach (reverse @cache1) {
		chomp;
		if (/^\s*$/ && $skip) {
			next;
		} else {
			$skip = 0;
		}
		push(@cache2, $_);
	}
	foreach (reverse @cache2) {
		$output .= "$_\n";
	}

	return $output;
}

sub switch_delimiter {
	my $data = shift;
	my $output;
	my $option_delimiter_string = return_option('delimiter', 'string', $option{dau_delimiter_string});

	foreach my $char (split //, $data) {
		$output .= $char . $option_delimiter_string;
	}
	return $output;
}

sub switch_dots {
	my $data = shift;

	$data =~ s/[.]*\s+/
	           if (rand(10) < 3) {
	               (rand(10) >= 5 ? ' ' : '')
	               .
	               ('...' . '.' x rand(5))
	               .
	               (rand(10) >= 5 ? ' ' : '')
	           } else { ' ' }
	          /egox;
	rand(10) >= 5 ? $data .= ' ' : 0;
	$data .= ('...' . '.' x rand(10));

	return $data;
}

sub switch_figlet {
	my $data = shift;
	my $skip = 1;
	my ($output, @fonts, %font, $font, @cache1, @cache2);

	if (-e $option{dau_figlet_path} && !(-f $option{dau_figlet_path})) {
		print_err('dau_figlet_path has to point to the figlet executable.');
		return;
	} elsif (!(-e $option{dau_figlet_path})) {
		print_err('figlet not found. Install it and set dau_figlet_path.');
		return;
	}

	if (return_option('figlet', 'font')) {
		$font = return_option('figlet', 'font');
	} else {
		while ($option{dau_figlet_fontlist} =~ /\s*([^,\s]+)\s*,?/g) {
			$font{$1} = 1;
		}
		foreach my $font (keys %{ $switches{combo}{figlet}{font} }) {
			if (lc($option{dau_figlet_fontpolicy}) eq 'allow') {
				push(@fonts, $font)
					unless ($font{$font});
			} elsif (lc($option{dau_figlet_fontpolicy}) eq 'deny') {
				push(@fonts, $font)
					if ($font{$font});
			} else {
				print_err('Invalid value for dau_figlet_fontpolicy.');
				return;
			}
		}
		if (@fonts == 0) {
			print_err('Cannot find figlet fonts.');
			return;
		}
		$font = $fonts[rand(@fonts)];
	}

	# Run figlet

	local(*HIS_IN, *HIS_OUT, *HIS_ERR);

	my $childpid = open3(*HIS_IN, *HIS_OUT, *HIS_ERR, $option{dau_figlet_path}, '-f', $font);

	print HIS_IN $data or return;
	close(HIS_IN) or return;

	my @errlines = <HIS_ERR>;
	my @outlines = <HIS_OUT>;
	close(HIS_ERR) or return;
	close(HIS_OUT) or return;

	waitpid($childpid, 0);
	if ($?) {
		print_err("That child exited with wait status of $?");
	}

	# Error during execution? Print errors and return

	unless (@errlines == 0) {
		print_err('Error during execution of figlet');
		foreach my $line (@errlines) {
			print_err($line);
		}
		return;
	}

	if ($option{dau_figlet_print_font}) {
		print_out("Using figlet font $font");
	}

	foreach (@outlines) {
		chomp;
		if (/^\s*$/ && $skip) {
			next;
		} else {
			$skip = 0;
		}
		push(@cache1, $_);
	}
	$skip = 1;
	foreach (reverse @cache1) {
		chomp;
		if (/^\s*$/ && $skip) {
			next;
		} else {
			$skip = 0;
		}
		push(@cache2, $_);
	}
	foreach (reverse @cache2) {
		$output .= "$_\n";
	}

	return $output;
}

sub switch_leet {
	my $data = shift;

	$_ = $data;

	s'fucker'f@#$er'gi;
	s/hacker/h4x0r/gi;
	s/sucker/sux0r/gi;
	s/fear/ph34r/gi;

	s/\b(\w+)ude\b/${1}00d/gi;
	s/\b(\w+)um\b/${1}00m/gi;
	s/\b(\w{3,})er\b/${1}0r/gi;
	s/\bdo\b/d00/gi;
	s/\bthe\b/d4/gi;
	s/\byou\b/j00/gi;

	tr/lLzZeEaAsSgGtTbBqQoOiIcC/11223344556677889900||((/;
	s/(\w)/rand(100) < 50 ? "\u$1" : "\l$1"/ge;

	return $_;
}

sub switch_me {
	my ($data, $channel) = @_;

	return $data;
}

# &switch_mix by Martin Kihlgren <zond@troja.ath.cx>
# slightly modified by myself

sub switch_mix {
	my $data = shift;
	my $output;

	while ($data =~ s/(\s*)([^\w]*)([\w]+)([^\w]*)(\s+[^\w]*\w+[^\w]*\s*)*/$5/) {
		my $prespace = $1;
		my $prechars = $2;
		my $w = $3;
		my $postchars = $4;
		$output = $output . $prespace . $prechars . substr($w,0,1);
		my $middle = substr($w,1,length($w) - 2);
		while ($middle =~ s/(.)(.*)/$2/) {
			if (rand() > 0.1) {
				$middle = $middle . $1;
			} else {
				$output = $output . $1;
			}
		}
		if (length($w) > 1) {
			$output = $output . substr($w, length($w) - 1, 1);
		}
		$output = $output . $postchars;
	}

	return $output;
}

sub switch_mixedcase {
	my $data = shift;

	$data =~ s/([[:alpha:]])/rand(100) < 50 ? uc($1) : lc($1)/ge;

	return $data;
}

sub switch_moron {
	my ($data, $channel_rec) = @_;
	my $output;
	my $option_eol_style = return_option('moron', 'eol_style', $option{dau_moron_eol_style});
	my $option_language  = $option{dau_language};

	################################################################################
	# -omega on
	################################################################################

	my $omega;

	if (return_option('moron', 'omega') eq 'on') {
		my @words = qw(omfg lol wtf);

		foreach (split / (?=\w+\b)/, $data) {
			if (rand(100) < 20) {
				$omega .= ' ' . $words[rand(@words)] . " $_";
			} else {
				$omega .= ' ' . $_;
			}
		}

		$omega =~ s/\s*,\s+\@/ @/g;
		$omega =~ s/^\s+//;
	}

	$_ = $omega || $data;

	################################################################################
	# 'nick: text' -> 'text @ nick'
	################################################################################

	my $old_list_delimiter = $option{dau_parse_special_list_delimiter};
	$option{dau_parse_special_list_delimiter} = ' ';
	my @nicks = split(/ /, switch_parse_special('@nicks', $channel_rec));
	$option{dau_parse_special_list_delimiter} = $old_list_delimiter;
	@nicks = map { quotemeta($_) } @nicks;

	{
		local $" = '|';
		eval { # Catch strange error
			s/^(@nicks): (.+)/$2 @ $1/;
		};
	}

	################################################################################
	# Preparations for "EOL modifications" later
	################################################################################

	# Remove puntuation marks at EOL and ensure there is a single space at EOL.
	# This is necessary because the EOL-styles 'new' and 'classic' put them at
	# EOL. If EOL-style is set to 'nothing' don't do this.

	s/\s*([,;.:?!])*\s*$// unless ($option_eol_style eq 'nothing');
	my $lastchar = $1;

	# Only whitespace? Remove it.

	s/^\s+$//;

	################################################################################
	# Substitutions for every language
	################################################################################

	tr/'/`/;

	# Dauify smileys

	{
		# Use of uninitialized value in concatenation (.) or string at...
		# (the optional dash ($1) in the regular expressions).
		# Thus turn off warnings

		no warnings;

		if ($option{dau_language} eq 'de') {
			if (int(rand(2))) {
				s/:(-)?\)/^^/go;
			} else {
				s/:(-)?\)/':' . $1 . ')))' . (')' x rand(10)) . ('9' x rand(4))/ego;
			}

			s/;(-)?\)/';' . $1 . ')))' . (')' x rand(10)) . ('9' x rand(4))/ego;
			s/:(-)?\(/':' . $1 . '(((' . ('(' x rand(10)) . ('8' x rand(4))/ego;
			s#(^|\s):(-)?/(\s|$)#$1 . ':' . $2 . '///' . ('/' x rand(10)) . ('7' x rand(4)) . $3#ego;
		} else {
			if (int(rand(2))) {
				s/:(-)?\)/^^/go;
			} else {
				s/:(-)?\)/':' . $1 . ')))' . (')' x rand(10)) . ('0' x rand(4))/ego;
			}

			s/;(-)?\)/';' . $1 . ')))' . (')' x rand(10)) . ('0' x rand(4))/ego;
			s/:(-)?\(/':' . $1 . '(((' . ('(' x rand(10)) . ('9' x rand(4))/ego;
		}
	}

	################################################################################
	# English text
	################################################################################

	if ($option_language eq 'en') {
		s/\bthe\b/teh/go;
	}

	################################################################################
	# German text
	################################################################################

	if ($option_language eq 'de') {

		# '*GG*' -> 'ÜGGÜ'
		{
			my @a = ('*', 'Ü');
			my $a = $a[int(rand(@a))];
			s/\*g\*/$a . 'ggg' . ('g' x rand(10)) . $a/egio;
		}

		# verbs

		s/\b(f)reuen\b/$1roien/gio;
		s/\b(f)reue\b/$1roie/gio;
		s/\b(f)reust\b/$1roist/gio;
		s/\b(f)reut\b/$1roit/gio;

		s/\b(f)unktionieren\b/$1unzen/gio;
		s/\b(f)unktioniere\b/$1unze/gio;
		s/\b(f)unktionierst\b/$1unzt/gio;
		s/\b(f)unktioniert\b/$1unzt/gio;

		s/\b(h)olen\b/$1ohlen/gio;
		s/\b(h)ole\b/$1ohle/gio;
		s/\b(h)olst\b/$1ohlst/gio;
		s/\b(h)olt\b/$1ohlt/gio;

		s/\b(k)onfigurieren\b/$1 eq 'k' ? 'confen' : 'Confen'/egio;
		s/\b(k)onfiguriere\b/$1 eq 'k' ? 'confe' : 'Confe'/egio;
		s/\b(k)onfigurierst\b/$1 eq 'k' ? 'confst' : 'Confst'/egio;
		s/\b(k)onfiguriert\b/$1 eq 'k' ? 'conft' : 'Conft'/egio;

		s/\b(l)achen\b/$1ölen/gio;
		s/\b(l)ache\b/$1öle/gio;
		s/\b(l)achst\b/$1ölst/gio;
		s/\b(l)acht\b/$1ölt/gio;

		s/\b(m)achen\b/$1 eq 'm' ? 'tun' : 'Tun'/egio;
		s/\b(m)ache\b/$1 eq 'm' ? 'tu' : 'Tu'/egio;
		s/\b(m)achst\b/$1 eq 'm' ? 'tust' : 'Tust'/egio;

		s/\b(n)erven\b/$1erfen/gio;
		s/\b(n)erve\b/$1erfe/gio;
		s/\b(n)ervst\b/$1erfst/gio;
		s/\b(n)ervt\b/$1erft/gio;

		s/\b(p)rojizieren\b/$1rojezieren/gio;
		s/\b(p)rojiziere\b/$1rojeziere/gio;
		s/\b(p)rojizierst\b/$1rojezierst/gio;
		s/\b(p)rojiziert\b/$1rojeziert/gio;

		s/\b(r)egistrieren\b/$1egestrieren/gio;
		s/\b(r)egistriere\b/$1egestriere/gio;
		s/\b(r)egistrierst\b/$1egestrierst/gio;
		s/\b(r)egistriert\b/$1egestriert/gio;

		s/\b(s)pazieren\b/$1patzieren/gio;
		s/\b(s)paziere\b/$1patziere/gio;
		s/\b(s)pazierst\b/$1patzierst/gio;
		s/\b(s)paziert\b/$1patziert/gio;

		# other

		s/\bdanke\b/
		  if (int(rand(2)) == 0) {
		      'thx'
		  } else {
		      'danks'
		  }
		 /ego;
		s/\bDanke\b/
		  if (int(rand(2)) == 0) {
		      'Thx'
		  } else {
		      'Danks'
		  }
		 /ego;

		s/\blol\b/
		  if (int(rand(2)) == 0) {
		      'löl'
		  } else {
		      'löllens'
		  }
		 /ego;
		s/\bLOL\b/
		  if (int(rand(2)) == 0) {
		      'LÖL'
		  } else {
		      'LÖLLENS'
		  }
		 /ego;

		s/\br(?:ü|ue)ckgrat\b/
		  if (int(rand(3)) == 0) {
		      'rückgrad'
		  } elsif (int(rand(3)) == 1) {
		      'rückrad'
		  } else {
		      'rückrat'
		  }
		 /ego;
		s/\bR(?:ü|ue)ckgrat\b/
		  if (int(rand(3)) == 0) {
		      'Rückgrad'
		  } elsif (int(rand(3)) == 1) {
		      'Rückrad'
		  } else {
		      'Rückrat'
		  }
		 /ego;

		s/\b(i)st er\b/$1ssa/gio;
		s/\bist\b/int(rand(2)) ? 'is' : 'iss'/ego;
		s/\bIst\b/int(rand(2)) ? 'Is' : 'Iss'/ego;

		s/\b(d)a(?:ss|ß) du\b/$1asu/gio;
		s/\b(d)a(?:ss|ß)\b/$1as/gio;

		s/\b(s)ag mal\b/$1amma/gio;
		s/\b(n)ochmal\b/$1omma/gio;
		s/(m)al\b/$1a/gio;

		s/\b(u)nd nun\b/$1nnu/gio;
		s/\b(n)un\b/$1u/gio;

		s/\b(s)oll denn\b/$1olln/gio;
		s/\b(d)enn\b/$1en/gio;

		s/\b(s)o eine\b/$1onne/gio;
		s/\b(e)ine\b/$1 eq 'e' ? 'ne' : 'Ne'/egio;

		s/\bkein problem\b/NP/gio;
		s/\b(p)roblem\b/$1rob/gio;
		s/\b(p)robleme\b/$1robs/gio;

		s/\b(a)ber\b/$1bba/gio;
		s/\b(a)chso\b/$1xo/gio;
		s/\b(a)dresse\b/$1ddresse/gio;
		s/\b(a)ggressiv\b/$1gressiv/gio;
		s/\b([[:alpha:]]{2,})st du\b/${1}su/gio;
		s/\b(a)nf(?:ä|ae)nger\b/$1 eq 'a' ? 'n00b' : 'N00b'/egio;
		s/\b(a)sozial\b/$1ssozial/gio;
		s/\b(a)u(?:ss|ß)er\b/$1user/gio;
		s/\b(a)utor/$1uthor/gio;
		s/\b(b)asta\b/$1 eq 'b' ? 'pasta' : 'Pasta'/egio;
		s/\b(b)illard\b/$1illiard/gio;
		s/\b(b)i(?:ss|ß)chen\b/$1ischen/gio;
		s/\b(b)ist\b/$1is/gio;
		s/\b(b)itte\b/$1 eq 'b' ? 'plz' : 'Plz'/egio;
		s/\b(b)lo(?:ss|ß)\b/$1los/gio;
		s/\b(b)(?:ox|(?:ü|ue)chse)\b/$1yxe/gio;
		s/\b(b)rillant\b/$1rilliant/gio;
		s/\b(c)hannel\b/$1 eq 'c' ? 'kanal' : 'Kanal'/egio;
		s/\b(c)hat\b/$1hatt/gio;
		s/\b(c)ool\b/$1 eq 'c' ? 'kewl' : 'Kewl'/egio;
		s/\b(d)(?:ä|ae)mlich\b/$1ähmlich/gio;
		s/\b(d)etailliert\b/$1etailiert/gio;
		s/\b(d)ilettantisch\b/$1illetantisch/gio;
		s/\b(d)irekt\b/$1ireckt/gio;
		s/\b(d)iskussion\b/$1isskusion/gio;
		s/\b(d)istribution/$1ystrubution/gio;
		s/\b(e)igentlich\b/$1igendlich/gio;
		s/\b(e)inzige\b/$1inzigste/gio;
		s/\b(e)nd/$1nt/gio;
		s/\b(e)ntschuldigung\b/$1 eq 'e' ? 'sry' : 'Sry'/egio;
		s/\b(f)ilm\b/$1 eq 'f' ? 'movie' : 'Movie'/egio;
		s/\b(f)lachbettscanner\b/$1lachbrettscanner/gio;
		s/\b(f)reu\b/$1roi/gio;
		s/\b(g)alerie\b/$1allerie/gio;
		s/\b(g)ay\b/$1hey/gio;
		s/\b(g)ebaren\b/$1ebahren/gio;
		s/\b(g)elatine\b/$1elantine/gio;
		s/\b(g)eratewohl\b/$1eradewohl/gio;
		s/\b(g)ibt es\b/$1ibbet/gio;
		s/\bgra([dt])/$1 eq 'd' ? 'grat' : 'grad'/ego;
		s/\bGra([dt])/$1 eq 'd' ? 'Grat' : 'Grad'/ego;
		s/\b(h)(?:ä|ae)ltst\b/$1älst/gio;
		s/\b(h)(?:ä|ae)sslich/$1äslich/gio;
		s/\b(h)aneb(?:ü|ue)chen\b/$1ahneb$2chen/gio;
		s/\b(i)mmobilie/$1mobilie/gio;
		s/\b(i)nteressant\b/$1nterressant/gio;
		s/\b(i)ntolerant\b/$1ntollerant/gio;
		s/\b(i)rgend/$1rgent/gio;
		s/\b(j)a\b/$1oh/gio;
		s/\b(j)etzt\b/$1ez/gio;
		s/\b(k)affee\b/$1affe/gio;
		s/\b(k)aputt\b/$1aput/gio;
		s/\b(k)arussell\b/$1arussel/gio;
		s/\b(k)iste\b/$1 eq 'k' ? 'byxe' : 'Byxe'/egio;
		s/\b(k)lempner\b/$1lemptner/gio;
		s/\b(k)r(?:ä|ae)nker\b/$1ranker/gio;
		s/\b(k)rise\b/$1riese/gio;
		s/\b(l)etal\b/$1ethal/gio;
		s/\b(l)eute\b/$1 eq 'l' ? 'ppl' : 'Ppl'/egio;
		s/\b(l)ibyen\b/$1ybien/gio;
		s/\b(l)izenz\b/$1izens/gio;
		s/\b(l)oser\b/$1ooser/gio;
		s/\b(l)ustig/$1ölig/gio;
		s/\b(m)aschine\b/$1aschiene/gio;
		s/\b(m)illennium\b/$1illenium/gio;
		s/\b(m)iserabel\b/$1ieserabel/gio;
		s/\b(m)it dem\b/$1im/gio;
		s/\b(m)orgendlich\b/$1orgentlich/gio;
		s/\b(n)(?:ä|ae)mlich\b/$1ähmlich/gio;
		s/\b(n)ein\b/$1eh/gio;
		s/\bnett\b/n1/gio;
		s/\b(n)ewbie\b/$100b/gio;
		s/\bnicht\b/int(rand(2)) ? 'net' : 'ned'/ego;
		s/\bNicht\b/int(rand(2)) ? 'Net' : 'Ned'/ego;
		s/\b(n)iveau/$1iwo/gio;
		s/\bok(?:ay)?\b/K/gio;
		s/\b(o)riginal\b/$1rginal/gio;
		s/\b(p)aket\b/$1acket/gio;
		s/\b(p)l(?:ö|oe)tzlich\b/$1lözlich/gio;
		s/\b(p)ogrom\b/$1rogrom/gio;
		s/\b(p)rogramm\b/$1roggie/gio;
		s/\b(p)rogramme\b/$1roggies/gio;
		s/\b(p)sychiater\b/$1sychater/gio;
		s/\b(p)ubert(?:ä|ae)t\b/$1upertät/gio;
		s/\b(q)uarz\b/$1uartz/gio;
		s/\b(q)uery\b/$1uerry/gio;
		s/\b(r)eferenz\b/$1efferenz/gio;
		s/\b(r)eparatur\b/$1eperatur/gio;
		s/\b(r)eply\b/$1eplay/gio;
		s/\b(r)essource\b/$1esource/gio;
		s/\b(r)(o)(t?fl)\b/$1 . ($2 eq 'o' ? 'ö' : 'Ö') . $3/egio;
		s/\b(r)(o)(t?fl)(o)(l)\b/$1 . ($2 eq 'o' ? 'ö' : 'Ö') . $3 . ($4 eq 'o' ? 'ö' : 'Ö') . $5/egio;
		s/\b(s)atellit\b/$1attelit/gio;
		s/\b(s)cherz\b/$1chertz/gio;
		s/\bsei([dt])\b/$1 eq 'd' ? 'seit' : 'seid'/ego;
		s/\bSei([dt])\b/$1 eq 'd' ? 'Seit' : 'Seid'/ego;
		s/\b(s)elig\b/$1eelig/gio;
		s/\b(s)eparat\b/$1eperat/gio;
		s/\b(s)eriosit(?:ä|ae)t\b/$1erösität/gio;
		s/\b(s)onst\b/$1onnst/gio;
		s/\b(s)orry\b/$1ry/gio;
		s/\b(s)pelunke\b/$1ilunke/gio;
		s/\b(s)piel\b/$1 eq 's' ? 'game' : 'Game'/egio;
		s/\b(s)tabil\b/$1tabiel/gio;
		s/\b(s)tandard\b/$1tandart/gio;
		s/\b(s)tegreif\b/$1tehgreif/gio;
		s/\b(s)ympathisch\b/$1ymphatisch/gio;
		s/\b(s)yntax\b/$1ynthax/gio;
		s/\b(t)era/$1erra/gio;
		s/\b(t)oler/$1oller/gio;
		s/\bto([td])/$1 eq 't' ? 'tod' : 'tot'/ego;
		s/\bTo([td])/$1 eq 't' ? 'Tod' : 'Tot'/ego;
		s/\b(u)ngef(?:ä|ae)hr\b/$1ngefär/gio;
		s/\bviel gl(?:ü|ue)ck\b/GL/gio;
		s/\b(v)ielleicht\b/$1ileicht/gio;
		s/\b(v)oraus/$1orraus/gio;
		s/\b(w)(?:ä|ae)re\b/$1ähre/gio;
		s/\bwa(h)?r/$1 eq 'h' ? 'war' : 'wahr'/ego;
		s/\bWa(h)?r/$1 eq 'h' ? 'War' : 'Wahr'/ego;
		s/\b(w)as du\b/$1asu/gio;
		s/\b(w)eil du\b/$1eilu/gio;
		s/\bweis(s)?/$1 eq 's' ? 'weis' : 'weiss'/ego;
		s/\bWeis(s)?/$1 eq 's' ? 'Weis' : 'Weiss'/ego;
		s/\b(w)enn du\b/$1ennu/gio;
		s/\b(w)ider/$1ieder/gio;
		s/\b(w)ieso\b/$1iso/gio;
		s/\b(z)iemlich\b/$1iehmlich/gio;
		s/\b(z)umindest\b/$1umindestens/gio;

		tr/üÜ/yY/;
		s/ei(?:ss?|ß)e?/ice/go;
		s/eife?/ive/go;

		if(return_option('moron', 'level') >= 1) {
			s/\b(u)nd\b/$1nt/gio;
			s/\b(h)at\b/$1att/gio;
			s/\b(n)ur\b/$1uhr/gio;
			s/\b(v)er(\w+)/$1 eq 'V' ? "Fa$2" : "fa$2"/egio;
			s/\b([[:alpha:]]+[b-np-tv-z])er\b/${1}a/go;
			s/\b([[:alpha:]]+)ck/${1}q/go;

			s/\b([fv])(?=[[:alpha:]]{2,})/
			  if (rand(10) <= 4) {
			      if ($1 eq 'f') {
			          'v'
			      }
			      else {
			          'f'
			      }
			  } else {
			      $1
			  }
			 /egox;
			s/\b([FV])(?=[[:alpha:]]{2,})/
			  if (rand(10) <= 4) {
			      if ($1 eq 'F') {
			          'V'
			      }
			      else {
			          'F'
			      }
			  } else {
			      $1
			  }
			  /egox;
			s#\b([[:alpha:]]{2,})([td])\b#
			  my $begin = $1;
			  my $end   = $2;
			  if (rand(10) <= 4) {
			      if ($end eq 't' && $begin !~ /t$/) {
			          "${begin}d"
			      } elsif ($end eq 'd' && $begin !~ /d$/) {
			          "${begin}t"
			      } else {
			          "${begin}${end}"
			      }
			  } else {
			      "${begin}${end}"
			  }
			 #egox;
			s/\b([[:alpha:]]{2,})ie/
			  if (rand(10) <= 4) {
			      "$1i"
			  } else {
			      "$1ie"
			  }
			 /egox;
		}
	}

	$data = $_;

	################################################################################
	# Swap characters with characters near at the keyboard
	################################################################################

	my %mark;
	my %chars;
	if ($option{dau_language} eq 'de') {
		%chars = (
		          'a' => [ 's' ],
		          'b' => [ 'v', 'n' ],
		          'c' => [ 'x', 'v' ],
		          'd' => [ 's', 'f' ],
		          'e' => [ 'w', 'r' ],
		          'f' => [ 'd', 'g' ],
		          'g' => [ 'f', 'h' ],
		          'h' => [ 'g', 'j' ],
		          'i' => [ 'u', 'o' ],
		          'j' => [ 'h', 'k' ],
		          'k' => [ 'j', 'l' ],
		          'l' => [ 'k', 'ö' ],
		          'm' => [ 'n' ],
		          'n' => [ 'b', 'm' ],
		          'o' => [ 'i', 'p' ],
		          'p' => [ 'o', 'ü' ],
		          'q' => [ 'w' ],
		          'r' => [ 'e', 't' ],
		          's' => [ 'a', 'd' ],
		          't' => [ 'r', 'z' ],
		          'u' => [ 'z', 'i' ],
		          'v' => [ 'c', 'b' ],
		          'w' => [ 'q', 'e' ],
		          'x' => [ 'y', 'c' ],
		          'y' => [ 'x' ],
		          'z' => [ 't', 'u' ],
		         );
	} else {
		%chars = (
		          'a' => [ 's' ],
		          'b' => [ 'v', 'n' ],
		          'c' => [ 'x', 'v' ],
		          'd' => [ 's', 'f' ],
		          'e' => [ 'w', 'r' ],
		          'f' => [ 'd', 'g' ],
		          'g' => [ 'f', 'h' ],
		          'h' => [ 'g', 'j' ],
		          'i' => [ 'u', 'o' ],
		          'j' => [ 'h', 'k' ],
		          'k' => [ 'j', 'l' ],
		          'l' => [ 'k', 'ö' ],
		          'm' => [ 'n' ],
		          'n' => [ 'b', 'm' ],
		          'o' => [ 'i', 'p' ],
		          'p' => [ 'o', 'ü' ],
		          'q' => [ 'w' ],
		          'r' => [ 'e', 't' ],
		          's' => [ 'a', 'd' ],
		          't' => [ 'r', 'z' ],
		          'u' => [ 'z', 'i' ],
		          'v' => [ 'c', 'b' ],
		          'w' => [ 'q', 'e' ],
		          'x' => [ 'y', 'c' ],
		          'y' => [ 't', 'u' ],
		          'z' => [ 'x' ],
		         );
	}

	# Do not replace one character twice
	# Therefore every replace-position will be marked

	unless (lc(return_option('moron', 'typo')) eq 'off') {
		for (0 .. length($data)) {
			$mark{$_} = 0;
		}

		for (0 .. rand(length($data))/20) {
			my $pos = int(rand(length($data)));
			pos $data = $pos;
			unless ($mark{$pos} == 1)  {
				no locale;
				if ($data =~ /\G([A-Za-z])/g) {
					my $matched = $1;
					my $replacement;
					if ($matched eq lc($matched)) {
						$replacement = $chars{$matched}[int(rand(@{ $chars{$matched} }))];
					} else {
						$replacement = uc($chars{$matched}[int(rand(@{ $chars{$matched} }))]);
					}
					if ($replacement !~ /^\s*$/) {
						substr($data, $pos, 1, $replacement);
						$mark{$pos} = 1;
					}
				}
			}
		}
	}

	################################################################################
	# Mix in some typos (swapping characters)
	################################################################################

	unless (lc(return_option('moron', 'typo')) eq 'off') {
		foreach my $word (split /([\s\n])/, $data) {
			if ((rand(100) <= 20) && length($word) > 1) {
				my $position_swap = int(rand(length($word)));
				if ($position_swap == 0) {
					$position_swap = 1;
				} elsif ($position_swap == length($word)) {
					$position_swap = length($word) - 1;
				}
				if (substr($word, $position_swap - 1, 1) eq uc(substr($word, $position_swap - 1, 1)) &&
				    substr($word, $position_swap, 1)     eq lc(substr($word, $position_swap, 1)))
				{
					(substr($word, $position_swap, 1), substr($word, $position_swap - 1, 1)) =
					(lc(substr($word, $position_swap - 1, 1)), uc(substr($word, $position_swap, 1)));
				} else {
					(substr($word, $position_swap, 1), substr($word, $position_swap - 1, 1)) =
					(substr($word, $position_swap - 1, 1), substr($word, $position_swap, 1));
				}
			}
			$output .= $word;
		}
	} else {
		$output = $_;
	}

	################################################################################
	# plenk
	################################################################################

	$output =~ s/(\w+)([,;.:?!]+)(\s+|$)/
	           if (rand(10) <= 8 || $3 eq '') {
	               "$1 $2$3"
	           } else {
	               "$1$2"
	           }
	          /egox;

	################################################################################
	# default behaviour: uppercase text
	################################################################################

	$output = uc($output) unless (return_option('moron', 'uppercase') eq 'off');

	################################################################################
	# do something at EOL
	################################################################################

	if ($option_eol_style ne 'nothing') {
		my $random = int(rand(100));

		$output .= ' ' unless ($output =~ /^\s*$/);

		# !!!!!!??????????!!!!!!!!!!11111

		if ($random <= 70 || $lastchar eq '!') {
			my @punct = qw(? !);
			$output .= $punct[rand(@punct)] x int(rand(5))
				for (1..15);

			if ($lastchar eq '?') {
				$output .= '?' x (int(rand(4))+1);
			} elsif ($lastchar eq '!') {
				$output .= '!' x (int(rand(4))+1);
			}

			if ($output =~ /\?$/) {
				if ($option{dau_language} eq 'de') {
					$output .= "ß" x int(rand(10));
				} else {
					$output .= "/" x int(rand(10));
				}
			} elsif ($output =~ /!$/) {
				$output .= "1" x int(rand(10));
			}
		}

		# ?¿?

		elsif ($random <= 85) {
			$output .= '?¿?';
		}

		# "=\n?"

		else {
			$output .= "=\n?";
		}
	}

	return $output;
}

sub switch_nothing {
	my $data = shift;

	return $data;
}

sub switch_parse_special {
	my ($data, $channel) = @_;

	return $data;
}

sub switch_reverse {
	my $data = shift;

	$data = reverse($data);

	return $data;
}

sub switch_stutter {
	my $data = shift;
	my $output;
	my @words = qw(eeeh oeeeh aeeeh);

	foreach (split / (?=\w+\b)/, $data) {
		if (rand(100) < 20) {
			$output .= ' ' . $words[rand(@words)] . ", $_";
		} else {
			$output .= ' ' . $_;
		}
	}

	$output =~ s/\s*,\s+\@/ @/g;

	for (1 .. rand(length($output)/5)) {
		pos $output = rand(length($output));
		$output =~ s/\G ([[:alpha:]]+)\b/ $1, $1/;
	}
	for (1 .. rand(length($output)/10)) {
		pos $output = rand(length($output));
		$output =~ s/\G([[:alpha:]])/$1 . ($1 x rand(3))/e;
	}

	$output =~ s/^\s+//;

	return $output;
}

sub switch_substitute {
	$_ = shift;

	my $basefile = return_option('substitute', 'file', $option{dau_files_substitute});
	my $file = "$option{dau_files_root_directory}/$basefile";

	if (-e $file && -r $file) {
		my $return = do $file;

		if ($@) {
			print_err("parsing $file failed: $@");
		}
		unless (defined($return)) {
			print_err("'do $file' failed");
		}
	}

	return $_;
}

sub switch_underline {
	my ($data, $channel) = @_;

	return $data;
}

sub switch_uppercase {
	my $data = shift;

	$data = uc($data);

	return $data;
}

sub switch_words {
	my $data = shift;
	my $output;
	my @numbers;

	if ($option{dau_words_range} =~ /^([1-9])-([1-9])$/) {
		my $x = $1;
		my $y = $2;
		unless ($x <= $y) {
			print_err('Invalid value for setting dau_words_range.');
			return;
		}
		if ($x == $y) {
			push(@numbers, $x);
		} elsif ($x < $y) {
			for (my $i = $x; $i <= $y; $i++) {
				push(@numbers, $i);
			}
		}
	} else {
		print_err('Invalid value for dau_words_range.');
		return;
	}
	my $random = $numbers[rand(@numbers)];
	while ($data =~ /((?:.*?(?:\s+|$)){1,$random})/g) {
		$output .= "$1\n"
			unless (length($1) == 0);
		$random = $numbers[rand(@numbers)];
	}

	$output =~ s/\s*$//;

	return $output;
}

################################################################################
# Helper subroutines
################################################################################

sub def_dau_cowsay_cowpath {
	my $cowsay = $ENV{COWPATH} || '/usr/share/cowsay/cows';
	chomp($cowsay);
	return $cowsay;
}

sub def_dau_cowsay_cowsay_path {
	my $cowsay = `which cowsay`;
	chomp($cowsay);
	return $cowsay;
}

sub def_dau_cowsay_cowthink_path {
	my $cowthink = `which cowthink`;
	chomp($cowthink);
	return $cowthink;
}

sub def_dau_figlet_fontpath {
	my $figlet = `figlet -I2`;
	chomp($figlet);
	return $figlet;
}

sub def_dau_figlet_path {
	my $figlet = `which figlet`;
	chomp($figlet);
	return $figlet;
}

sub cowsay_cowlist {
	my $cowsay_cowpath = shift;

	# clear cowlist

	%{ $switches{combo}{cowsay}{cow} } = ();

	# generate new list

	while (<$cowsay_cowpath/*.cow>) {
		my $cow = (fileparse($_, qr/\.[^.]*/))[0];
		$switches{combo}{cowsay}{cow}{$cow} = 1;
	}
}

sub figlet_fontlist {
	my $figlet_fontpath = shift;

	# clear fontlist

	%{ $switches{combo}{figlet}{font} } = ();

	# generate new list

	while (<$figlet_fontpath/*.flf>) {
		my $font = (fileparse($_, qr/\..*/))[0];
		$switches{combo}{figlet}{font}{$font} = 1;
	}
}

sub fix {
	my $string = shift;
	$string =~ s/^\t+//gm;
	return $string;
}

sub parse_text {
	my ($data, $channel_rec) = @_;
	my $output;

	$command_out_activated = 0;
	$command_out           = 'MSG';
	$counter_switches      = 0;
	$daumode_activated     = 0;
	$print_message         = 0;
	%queue                 = ();

	OUTER: while ($data =~ /^--(\w+) */g) {

		my $first_level_option  = $1;

		# If its the first time we are in the OUTER loop, check
		# if the first level option is one of the few options,
		# which must not be combined.

		if (ref($switches{nocombo}{$first_level_option}{'sub'}) && $counter_switches == 0) {

			$data =~ s/^--\w+ *//;

			# found a first level option

			$queue{$counter_switches}{$first_level_option} = { };

			# Check for second level options and third level options.
			# Get all of them and put theme in the
			# $queue hash

			while ($data =~ /^-(\w+) ('.*?(?<![\\])'|\S+) */g) {

				my $second_level_option = $1;
				my $third_level_option  = $2;

				$third_level_option =~ s/^'//;
				$third_level_option =~ s/'$//;
				$third_level_option =~ s/\\'/'/g;

				# If $switches{nocombo}{$first_level_option}{$second_level_option}{'*'}:
				# The user can give any third_level_option on the commandline

				my $any_option =
				$switches{nocombo}{$first_level_option}{$second_level_option}{'*'} ? 1 : 0;

				if ($switches{nocombo}{$first_level_option}{$second_level_option}{$third_level_option} ||
				    $any_option)
				{
					$queue{$counter_switches}{$first_level_option}{$second_level_option} = $third_level_option;
				}

				$data =~ s/^-(\w+) ('.*?(?<![\\])'|\S+) *//;
			}

			# initialize some values

			foreach my $second_level_option (keys(%{ $switches{nocombo}{$first_level_option} })) {
				if (!defined($queue{'0'}{$first_level_option}{$second_level_option})) {
					$queue{'0'}{$first_level_option}{$second_level_option} = '';
				}
			}

			# All done. Run the subroutine

			$output = &{ $switches{nocombo}{$first_level_option}{'sub'} }($data, $channel_rec);

			return $output;
		}

		# Check for all those options that can be combined.

		elsif (ref($switches{combo}{$first_level_option}{'sub'})) {

			$data =~ s/^--\w+ *//;

			# found a first level option

			$queue{$counter_switches}{$first_level_option} = { };

			# Check for second level options and
			# third level options. Get all of them and put them
			# in the $queue hash

			while ($data =~ /^-(\w+) ('.*?(?<![\\])'|\S+) */g) {

				my $second_level_option = $1;
				my $third_level_option  = $2;

				$third_level_option =~ s/^'//;
				$third_level_option =~ s/'$//;
				$third_level_option =~ s/\\'/'/g;

				# If $switches{combo}{$first_level_option}{$second_level_option}{'*'}:
				# The user can give any third_level_option on the commandline

				my $any_option =
				$switches{combo}{$first_level_option}{$second_level_option}{'*'} ? 1 : 0;

				# known option => Put it in the hash

				if ($switches{combo}{$first_level_option}{$second_level_option}{$third_level_option}
			            || $any_option)
				{
					$queue{$counter_switches}{$first_level_option}{$second_level_option} = $third_level_option;
					$data =~ s/^-(\w+) ('.*?(?<![\\])'|\S+) *//;
				} else {
					last OUTER;
				}
			}

			# increase counter

			$counter_switches++;
		}

		else {
			last OUTER;
		}
	}

	# initialize some values

	for (my $i = 0; $i < $counter_switches; $i++) {
		foreach my $first_level (keys(%{ $queue{$i} })) {
			if (ref($switches{combo}{$first_level})) {
				foreach my $second_level (keys(%{ $switches{combo}{$first_level} })) {
					if (!defined($queue{$i}{$first_level}{$second_level})) {
						$queue{$i}{$first_level}{$second_level} = '';
					}
				}
			}
		}
	}

	# text to subroutines

	$output = $data;

	# If theres no text left over, take one item of dau_random_messages

	if ($output eq '') {
		$output = return_random_list_item($option{dau_standard_messages});
	}

	# No options? Get options from setting dau_standard_options and run
	# parse_text() again

	if (keys(%queue) == 0) {

		if (!$counter_subroutines) {
			print_out("No options given, hence using the value of the setting %9dau_standard_options%9 and that is %9$option{dau_standard_options}%9", $channel_rec);
			$counter_subroutines++;
			$output = parse_text("$option{dau_standard_options} $output", $channel_rec);
		} else {
			print_err('Invalid value for setting dau_standard_options. ' .
			          'Will use %9--moron%9 instead!');
			$output =~ s/^\Q$option{dau_standard_options}\E //;
			$output = parse_text("--moron $output", $channel_rec);
		}

	} else {

		$counter_switches = 0;

		for (keys(%queue)) {
			my ($first_level_option) = keys %{ $queue{$counter_switches} };
			$output = &{ $switches{combo}{$first_level_option}{'sub'} }($output, $channel_rec);
			$counter_switches++;
		}
	}

	# reset subcounter

	$counter_subroutines = 0;

	# return text

	return $output;
}

sub print_err {
	my $text = shift;

	warn "DAU.pm error: $text";
}

sub print_out {
	my ($text, $channel_rec) = @_;

	if ($option{dau_silence}) {
		return;
	}

	foreach my $line (split /\n/, $text) {
		my $message = "%9dau.pl%9: $line";
		if (defined($channel_rec) && $channel_rec) {
			$channel_rec->print($message);
		} else {
			print CLIENTCRAP $message;
		}
	}
}

# return_option('firstlevel', 'secondlevel'):
#
# If "--firstlevel -secondlevel value" given on the commandline, return 'value'.
#
# return_option('firstlevel', 'secondlevel', 'default value'):
#
# If "--firstlevel -secondlevel value" not given on the commandline, return
# 'default value'.
sub return_option {
	if (@_ == 2) {
		return $queue{$counter_switches}{$_[0]}{$_[1]};
	} elsif (@_ == 3) {
		if (length($queue{$counter_switches}{$_[0]}{$_[1]}) > 0) {
			return $queue{$counter_switches}{$_[0]}{$_[1]};
		} else {
			return $_[2];
		}
	} else {
		return 0;
	}
}

sub return_random_list_item {
	my $arg = shift;
	my @strings;

	my $item;
	while ($arg =~ /([^,]+)/g) {
		my $match = $1;
		if ($match =~ s/\\$//) {
			$item .= "$match,";
		} else {
			$item .= $match;
			$item =~ s/^\s*//;
			$item =~ s/\s*$//;
			push @strings, $item;
			$item = "";
		}
	}

	if (@strings == 0) {
		return;
	} else {
		return $strings[rand(@strings)];
	}
}

sub time_diff_verbose {
	my ($sub1, $sub2) = @_;

	my $difference = $sub1 - $sub2;
	$difference *= (-1) if ($difference < 0);
	my $seconds = $difference % 60;
	$difference = ($difference - $seconds) / 60;
	my $minutes = $difference % 60;
	$difference = ($difference - $minutes) / 60;
	my $hours   = $difference % 24;
	$difference = ($difference - $hours) / 24;
	my $days    = $difference % 7;
	my $weeks   = ($difference - $days) / 7;

	my $time;
	$time  = "$weeks week"     . ($weeks   == 1 ? "" : "s") . ", " if ($weeks);
	$time .= "$days day"       . ($days    == 1 ? "" : "s") . ", " if ($weeks || $days);
	$time .= "$hours hour"     . ($hours   == 1 ? "" : "s") . ", " if ($weeks || $days || $hours);
	$time .= "$minutes minute" . ($minutes == 1 ? "" : "s") . ", " if ($weeks || $days || $hours || $minutes);
	$time .= "$seconds second" . ($seconds == 1 ? "" : "s")        if ($weeks || $days || $hours || $minutes || $seconds);

	return $time;
}

sub time_parse {
	my $time = $_[0];
	my $parsed_time = 0;

	# milliseconds
	while ($time =~ s/(\d+)\s*(?:milliseconds|ms)//g) {
		$parsed_time += $1;
	}
	# seconds
	while ($time =~ s/(\d+)\s*s(?:econds?)?//g) {
		$parsed_time += $1 * 1000;
	}
	# minutes
	while ($time =~ s/(\d+)\s*m(?:inutes?)?//g) {
		$parsed_time += $1 * 1000 * 60;
	}
	# hours
	while ($time =~ s/(\d+)\s*h(?:ours?)?//g) {
		$parsed_time += $1 * 1000 * 60 * 60;
	}
	# days
	while ($time =~ s/(\d+)\s*d(?:ays?)?//g) {
		$parsed_time += $1 * 1000 * 60 * 60 * 24;
	}
	# weeks
	while ($time =~ s/(\d+)\s*w(?:eeks?)?//g) {
		$parsed_time += $1 * 1000 * 60 * 60 * 24 * 7;
	}

	if ($time !~ /^\s*$/) {
		print_err('Error while parsing the date!');
		return 0;
	}

	return $parsed_time;
}

1;

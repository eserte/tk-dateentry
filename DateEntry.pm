#
# DateEntry lets the user select a date from a drop-down calendar.
#
# Author: Hans J. Helgesen, October 1999.
#
# Maintainer is now Slaven Rezic <slaven.rezic@berlin.de>
#
# To contact the original author:
# <hans.helgesen@novit.no>
#
# See end of this file for documentation.
#
package Tk::DateEntry;

use vars qw($VERSION);

$VERSION = '1.34';

use Tk;
use strict;
use Carp;
use Time::Local;
BEGIN { eval 'use POSIX qw(strftime)'; warn $@ if $@ }

require Tk::Frame;

use base qw(Tk::Frame);
Construct Tk::Widget 'DateEntry';

sub ClassInit {
    my($class, $mw) = @_;
    $class->SUPER::ClassInit($mw);
    $mw->bind($class, "<Button-1>" => 'buttonDown');
}

sub Populate {
    my ($w, $args) = @_;

    $w->SUPER::Populate($args);

    # entry widget and arrow button
    my $e = $w->Entry;
    my $b = $w->Button(-bitmap => '@' . Tk->findINC("cbxarrow.xbm"));
    my $tl = $w->{_toplevel} = $w->Toplevel(-bd=>2,-relief=>'raised');

    $w->Advertise("entry" => $e);
    $w->Advertise("arrow" => $b);

    $tl->transient($w);
    $tl->overrideredirect(1);
    $tl->OnDestroy(sub { $w->{_status} = 'done' }); # XXX really needed?

    $b->pack(-side => "right", -padx => 0);
    $e->pack(-side => "right", -fill => 'x', -expand => 1, -padx => 0);

    # other initializations
    $b->bind("<Button-1>", [ $w => 'buttonDown' ]);
    $b->bind("<space>", [ $w => 'buttonDown' ]);
    $b->bind("<Key-Return>", [ $w => 'buttonDown' ]);
    $e->bind("<Key-Return>", [ $w => 'buttonDown' ]);
#XXX sigh
#    $w->bind("<FocusOut>", sub { return if $w->grabCurrent eq $w; warn $w->focusCurrent,"\n";return if defined $w->focusCurrent && $tl eq $w->focusCurrent; my $e = shift->XEvent;warn "focusout " . $e->d . " " . $e->m . "\n";$w->popDown });
#    $w->bind("<FocusOut>", [ $w => 'popDown' ]);
#    $w->bind("<FocusOut>", sub { warn $w->focusCurrent,"\n";return if defined $w->focusCurrent && $tl eq $w->focusCurrent; my $e = shift->XEvent;warn "focusout " . $e->d . " " . $e->m . "\n";$w->popDown });
    $w->bind("<FocusOut>", sub { my $e = shift->XEvent;
				 my $wout = sub { if (UNIVERSAL::isa($_[0],"Tk::Label")) { $_[0]->cget(-text) } else { $_[0] } };
				 warn "focusout " . $e->d . " " . $e->m . "\n";
				 warn "focusc=".$wout->($w->focusCurrent).", grabc=".$wout->($w->grabCurrent)."\n";
				 $w->popDown;
			     });
#    $tl->bind("<FocusOut>", [ $w => 'popDown' ]);

    # Create the buttons on the dropdown.
    my $fr = $w->{_frame} = $tl->Frame->pack(-anchor=>'n');

    # check whether Tk::FireButton is installed
    my $Button = eval { require Tk::FireButton; 1 } ? 'FireButton' : 'Button';

    # 1. Previous month:
    $w->{_backbutton}=$fr->$Button(-text=>'<<',-pady=>1,-padx=>1,-bd=>1,
				   -command=> ['prevMonth', $w])
	->grid(-row=>0,-column=>0);

    # 2. Label to put the monthname in:
    $w->{_monthlabel} = $fr->Label->grid(-row=>0,-column=>1,-columnspan=>5);

    # 3. Next month:
    $w->{_nextbutton}=$fr->$Button(-text=>'>>',-pady=>1,-padx=>1,-bd=>1,
				   -command=>['nextMonth', $w])
	->grid(-row=>0,-column=>6);

    # 4. Dayname labels:
    for (0..6) {
	$w->{_daylabel}->[$_] = $fr->Label->grid(-column=>$_,-row=>1);
    }

    # 5. Daybuttons. Note that we create button for six weeks, since it
    #    is possible that a month might span over six different weeks.
    #    The text on the buttons are just a dummy to force them to the
    #    correct size. When the calendar is popped up, the right text
    #    is inserted an unused buttons are gridForget'ed.
    for my $week (0..5) {
	for my $wday (0..6) {
	    $w->{_daybutton}->[$week]->[$wday] =
		$fr->Button(-bd=>1, -padx=>1, -pady=>1, -text=>'00',
			    -command => ['selectDay', $w, $week, $wday])
		    ->grid(-row=>$week+2,-column=>$wday,-sticky=>'nsew');
	}
    }
    $tl->withdraw;

    $w->{_popped} = 0;

    $w->Delegates(DEFAULT => $e);

    $w->ConfigSpecs
	(-arrowimage  => [{-image => $b}, qw/arrowImage ArrowImage/],
 	 -variable    => "-textvariable",
         -dateformat  => [qw/PASSIVE dateFormat DateFormat 1/],
	 -background  => [qw/METHOD background Background/],
# XXX should the class for these be Background?
	 -buttonbackground
           	      => [qw/METHOD buttonBackground ButtonBackground/],
	 -boxbackground
                      => [qw/METHOD boxBackground BoxBackground/],
	 -todaybackground
                      => [qw/PASSIVE todayBackground TodayBackground/],
	 -font        => [qw/DESCENDANTS font Font/],
	 -daynames    => [qw/PASSIVE daynames Daynames/,[qw/S M Tu W Th F S/]],
	 -weekstart   => [qw/PASSIVE weekstart Weekstart 0/],
	 -formatcmd   => [qw/CALLBACK formatCmd FormatCmd/,
			  ['defaultFormat',$w]],
	 -parsecmd    => [qw/CALLBACK parseCmd ParseCmd/,
			  ['defaultParse', $w]],
	 -headingfmt  => [qw/PASSIVE headingFmt HeadingFmt/, '%B %Y'],
	 -state       => [qw/METHOD state State normal/],
	 -width       => [$e, undef, undef, 10],
	 DEFAULT      => [$e] );
}

#---------------------------------------------------------------------------
# Configuration methods:
#
# -state works like the BrowseEntry's -state, in addition to 'normal'
# and 'disabled', 'readonly' forces the user to select from the dropdown.
sub state {
    my $w = shift;

    unless (@_) {
        return ($w->{'de_state'});
    } else {
        my $state = $w->{'de_state'} = shift;

	if ($state eq "readonly" ) {
	    $w->Subwidget("entry")->configure( -state => "disabled" );
	    $w->Subwidget("arrow")->configure( -state => "normal" );
	} else {
	    $w->Subwidget("entry")->configure( -state => $state );
	    $w->Subwidget("arrow")->configure( -state => $state );
	}
    }
}

# -background sets the background of the entry and arrow subwidgets.
#
sub background
{
    my $w = shift;

    unless (@_) {
        return ($w->{'de_background'});
    } else {
        my $color = $w->{'de_background'} = shift;

	foreach (qw/entry arrow/) {
	    $w->Subwidget($_)->configure(-background=>$color);
	}
    }
}

# -buttonbackground sets the background on all buttons in the widget.
#
sub buttonbackground
{
    my $w = shift;

    unless (@_) {
        return ($w->{'de_buttonbackground'});
    } else {
        my $color = $w->{'de_buttonbackground'} = shift;

	foreach (qw/_backbutton _nextbutton/) {
	    $w->{$_}->configure('-background'=>$color);
	}
    }
}

sub boxbackground
{
    my $w = shift;

    unless (@_) {
        return ($w->{'de_boxbackground'});
    } else {
        my $color = $w->{'de_boxbackground'} = shift;
	foreach (qw/_frame _toplevel _monthlabel/) {
	    $w->{$_}->configure('-background'=>$color);
	}
	foreach (@{$w->{_daylabel}}) {
	    $_->configure('-background'=>$color);
	}
    }
}

sub configure
{
    my ($w, %args) = @_;

    # Check validity of option values.
    if (defined($args{-weekstart}) &&
	($args{-weekstart} < 0 || $args{-weekstart} > 6))
    {
	carp ("-weekstart must be between 0 and 6");
	delete $args{-weekstart};  # Ignore -weekstart
    }

    if (defined($args{-dateformat}) &&
	($args{-dateformat} < 1 || $args{-dateformat} > 3))
    {
	carp ("-dateformat must be between 1 and 3");
	delete $args{-dateformat};  # Ignore -dateformat
    }


    $w->SUPER::configure(%args);

    if (defined($args{-daynames}) || defined($args{-weekstart})) {
	# Refresh the daynames heading whenever -daynames or -weekstart
	# changes.
	my $daynames = $w->cget('-daynames');
	my $weekstart = $w->cget('-weekstart');

	for (0..6) {
	    $w->{_daylabel}->[$_]->configure
		(-text => $daynames->[($_ + $weekstart)%7]);
	}
    }
}

#---------------------------------------------------------------------------
# Whenever someone pushes the arrow.....

sub buttonDown
{
    my ($w) = @_;
    my $tl = $w->{_toplevel};

    return if $w->cget('-state') eq 'disabled';

    if ($w->{_popped}) {         # If already visible, pop down.
	return $w->popDown;
    }

    # Popup the widget.
    $w->popUp;

    $w->{_oldgrab} = $w->grabSave;
    $w->grabGlobal;               # Start processing......

    $w->readContent;             # Tries to read the current content of
                                  # entry, set default if empty.

    $w->{_status} = '';

    my ($today_d,$today_m,$today_y) = (localtime)[3,4,5];
    $today_m++;
    $today_y+=1900;

    while ($w->{_status} ne 'done') {
	# getCalendar returns a two-dimensional array, each row in the
	# array represents a week, each column the days of the week. If
	# an element has a value, the value is the day number of the month.
	#
	my $cal = $w->getCalendar;

	my $monthlabel = (defined &strftime
			  ? strftime($w->cget('-headingfmt'),0,0,0,1,
				     $w->{_month}-1,$w->{_year}-1900)
			  : $w->{_month} . "/" . $w->{_year}
			 );
	$w->{_monthlabel}->configure(-text=>$monthlabel);

	for my $week (0..5) {
	    for my $wday (0..6) {
		my $button = $w->{_daybutton}->[$week]->[$wday];
		my $mday = $cal->[$week]->[$wday];

		if (defined($mday)) {
		    # Set the buttons text to $mday, call grid() to make
		    # sure the button is displayed.
		    #
		    my $bckg = $w->cget('-buttonbackground') ||
			       ($button->configure('-background'))[3];

		    if ($mday == $today_d &&
			$w->{_month}==$today_m &&
			$w->{_year}==$today_y)
		    {
			# Special background for TODAY.
			$bckg = $w->cget('-todaybackground') ||
			        ($button->configure('-background'))[3];
		    }
		    $button->configure(-text => sprintf ("%2d", $mday),
				       -background => $bckg);
		    $button->grid
			(-column=>$wday,-row=>$week+2,-sticky=>'nsew');
		} else {
		    # This day does not exists in the given month. Remove
		    # the button.
		    #
		    $button->gridForget;
		}
	    }
	}

	# Wait for something to happen...
	$w->waitVariable(\$w->{_status});
    }

    $w->popDown;
}

#-------------------------------------------------------------------
# Displays the calendar window.
#
sub popUp
{
    my ($w) = @_;

    my $e = $w->Subwidget("entry");
    my $tl = $w->{_toplevel};
    my ($x, $y);

    # When the dislayed month changes, the number of weeks displayed might
    # change (minimum four, maximum six). To keep the size of the window
    # constant, we'll first put a dummy-button in each row, then "freeze"
    # the window size. This can't be done before now, since configure(-font)
    # might cause the size to change at any time.
    #
    $tl->packPropagate(1);         # Allow resize
    foreach (0..5) {
	$w->{_daybutton}->[$_]->[0]->grid(-column=>0,-row=>($_+2));
    }
    $tl->update;                   # Calculate size, since the window
                                   # is still withdrawn, the user won't see
                                   # anything.
    $tl->packPropagate(0);         # Freeze....


    my ($th,$tw) = ($tl->reqheight, $tl->reqwidth);
    my ($ex,$ey) = ($e->rootx, $e->rooty);
    my ($eh,$ew) = ($e->height, $e->width);
    my ($rh,$rw) = ($w->vrootheight, $w->vrootwidth);

    # Calculate vertical position first.
    # Best position is below the entry widget.
    if ($ey + $eh + $th < $rh) {
	$y = $ey + $eh;
    } else {
	# Above entry.
	$y = $ey - $th;
    }
    if ($y < 1) {
	$y = 1;
    }

    # Horizontal, best position is directly below/above the entry.
    if ($ex + $tw < $rw) {
	$x = $ex;
    } else {
	$x = 1;
    }
    if ($x < 1) {
	$x = 1;
    }
    $tl->geometry(sprintf("+%d+%d",$x,$y));

    $tl->deiconify;
    $tl->raise;

    $w->Subwidget("entry")->focus;
    $w->{_popped} = 1;
}

#----------------------------------------------------------------------
# Reads the current content of the widget and parses it to retrieve the
# year/month/day. Sets default values if something is missing or is
# invalid.
#
sub readContent
{
    my ($w) = @_;
    my $e = $w->Subwidget("entry");

    # Get year/month/day from the entry widget.
    #
    my ($year,$month,$day) = $w->Callback(-parsecmd => $e->get);
    foreach ($year,$month,$day) {
	unless (m/^\d+$/) {
	    undef $_;
	}
    }

    # Get todays date...
    my ($today_m,$today_y) = (localtime)[4,5];
    $today_y+=1900;
    $today_m++;

    unless (defined($month) && $month >= 1 && $month <= 12) {
	$month = $today_m;
    }

    $year = $today_y unless defined($year);
    if ($year < 100) {
	# One or two digit year. Try to find a reasonable value for
	# century by using a "100 years window".
	my $cc = int($today_y / 100); # Try current century
	my $yyyy = sprintf "%02d%02d", $cc, $year;

	if ($yyyy > ($today_y + 50)) {
	    $yyyy -= 100;  # More than 50 years in the future, must be
	                   # prev. century
	} elsif ($yyyy < ($today_y - 50)) {
	    $yyyy += 100;  # More that 50 years ago, must be next cent.
	}
	$year = $yyyy;
    }
    unless ($year =~ m/^\d+$/) {
	$year = $today_y;
    }

    $w->{_month} = $month;
    $w->{_year} = $year;
}

#--------------------------------------------------------------------
# Default date parse routine. Called unless -parsecmd is specified.
# (rememember to update check in configure() if more dateformats are
# added).
sub defaultParse
{
    my ($w, $str) = @_;

    my ($m,$d,$y);

    $_ = $w->cget('-dateformat');

    $str =~ s/\s//g;

    /^1$/ && (($m,$d,$y) = (split '/', $str));
    /^2$/ && (($y,$m,$d) = (split '/', $str));
    /^3$/ && (($d,$m,$y) = (split '/', $str));

    return ($y,$m,$d);
}

#--------------------------------------------------------------------
# Default date format routine. Called unless -formatcmd is specified.
# (rememember to update check in configure() if more dateformats are
# added).
sub defaultFormat
{
    my ($w, $y, $m, $d) = @_;

    $_=$w->cget('-dateformat');
    if (/^1$/) {
	sprintf("%02d/%02d/%04d", $m, $d, $y);
    } elsif (/^2$/) {
	sprintf("%04d/%02d/%02d", $y, $m, $d);
    } elsif (/^3$/) {
	sprintf("%02d/%02d/%04d", $d, $m, $y);
    }
}

#-----------------------------------------------------------------------
# Returns a calendar for the month given by $w->{_month} and {_year}.
# The calendar is returned as a 6 * 7 two-dimensional array. Each row in the
# array represents a week, each column a weekday.
#
# EXAMPLE: October 1999 (assume -weekstart => 0):
#
#  undef undef undef undef undef   1     2
#    3     4     5     6     7     8     9
#   10    11    12    13    14    15    16
#   17    18    19    20    21    22    23
#   24    25    26    27    28    29    30
#   31   undef undef undef undef undef undef
#
sub getCalendar
{
    my ($w) = @_;

    my $week=0;
    my $cal=[];

    for my $mday (1..31) {
	my ($m,$y,$wday) =
	    (localtime(timelocal(0,0,0,
				 $mday,
				 $w->{_month}-1,
				 $w->{_year})))[4..6];
	$m++;

	$wday = ($wday - $w->cget('-weekstart')) % 7;
	if ($m == $w->{_month}) { # Still the same month?
	    $cal->[$week]->[$wday]=$mday;
	    if ($wday == 6) {
		$week++;
	    }
	}
    }

    return $cal;
}

#--------------------
# Hide the window....
#
sub popDown
{
    my ($w) = @_;
    if ($w->{_popped}) {
	$w->{_popped} = 0;
	$w->grabRelease;
	if ($w->{_oldgrab}) {
	    $w->{_oldgrab}->();
	    delete $w->{_oldgrab};
	}
	$w->{_toplevel}->withdraw;
	$w->{_status} = 'done';
    }
}


#------------------
# BUTTON CALLBACKS:
#------------------
#
# Called when a day button is pressed. Sets the entry value, pops down
# the window.
#
sub selectDay
{
    my ($w, $week, $wday) = @_;


    my $e = $w->Subwidget("entry");
    my $mday = $w->{_daybutton}->[$week]->[$wday]->cget('-text');

    if ($w->cget('-state') eq 'readonly') {
	$e->configure('-state'=>'normal');
    }
    $e->delete('0','end');
    $e->insert('end',
	       $w->Callback(-formatcmd=>$w->{_year},$w->{_month}, $mday));

    if ($w->cget('-state') eq 'readonly') {
	$e->configure('-state'=>'disabled');
    }

    $w->popDown;

}

# Increment month number (or year)
sub nextMonth
{
    my ($w) = @_;

    if ($w->{_month} == 12) {
	$w->{_year}++;
	$w->{_month} = 1;
    } else {
	$w->{_month}++;
    }
    $w->{_status} = 'new';
}

# Decrement month number (or year)
sub prevMonth
{
    my ($w) = @_;

    if ($w->{_month} == 1) {
	$w->{_year}--;
	$w->{_month} = 12;
    } else {
	$w->{_month}--;
    }
    $w->{_status} = 'new';
}

1;

__END__



=head1 NAME

Tk::DateEntry - Drop down calendar widget for selecting dates.

=head1 SYNOPSIS

$dateentry = $parent->DateEntry (<options>);

=head1 DESCRIPTION

Tk::DateEntry is a drop down widget for selecting dates. It looks like
the BrowseEntry widget with an Entry followed by an arrow button, but
in stead of displaying a Listbox the DateEntry displays a calendar
with buttons for each date. The calendar contains buttons for browsing
through the months.

When the drop down is opened, the widget will try to read the current
content of the widget (the -textvariable), and display the month/year
specified. If the variable is entry, or contains invalid data, the
current month is displayed. If one or two digit year is specified,
the widget tries to guess the correct century by using a "100 year
window".

=head1 REQUIREMENTS

Tk::DateEntry requires Time::Local and POSIX (strftime)
(and basic Perl/Tk of course....)

=head1 OPTIONS

=over 4

=item -arrowimage => image

Use alternative image for the arrow button.

=item -dateformat => integer

Specify dateformat to use:

=over 4

=item

1 = MM/DD/YYYY - default.

=item

2 = YYYY/MM/DD

=item

3 = DD/MM/YYYY

=back

See also "DATE FORMATS" below.

=item -parsecmd => \&callback

In stead of using one of the builtin dateformats, you can specify your
own by supplying a subroutine for parsing (-parsecmd) and formatting
(-formatcmd) of the date string. These options overrides -dateformat.
See "DATE FORMATS" below.

=item -formatcmd => \&callback

See -parsecmd above and "DATE FORMATS" below.

=item -background => color

Sets the background color for the Entry subwidget. Note that
the dropdown calendar is not affected by this option. See also
-boxbackground, -buttonbackground and -todaybackground.

=item -buttonbackground => color

Sets the background color for all button in the dropdown calendar.

=item -boxbackground => color

Sets the background color for the dropdown widget (not including the buttons).

=item -todaybackground => color

Sets the background color for the button representing the current date.

=item -font => font

Sets the font for all subwidgets.

=item -daynames => [qw/Sun Mon Tue Wed Thu Fri Sat/]

Specifies the daynames which is used in the calendar heading.
The default is [qw/S M Tu W Th F S/]. Note that the array MUST begin
with the name of Sunday, even if -weekstart specifies something else
than 0 (which is Sunday). See also "WEEKS" below.

=item -weekstart => number

Use this if you don't want the weeks to start on Sundays. Specify a number
between 0 (Sunday) and 6 (Saturday). See "WEEKS" below.

=item -headingfmt => string

Format for the Month name heading. The month name heading is created by
calling strftime(format,0,0,0,1,month,year). Default format is '%B %Y'.
Note that only month and year will have sensible values, including
day and/or time in the heading is possible, but it makes no sense.

If POSIX.pm is not available then this option has no effect and the
month name heading format will just be "%m/%Y".

=item -state => string

'normal', 'disabled' or 'readonly'. The latter forces the user to use
the drop down, editing in the Entry subwidget is disabled.

=item -width => number

Width of the Entry subwidget, default is 10 (which fits the default
date format MM/DD/YYYY).

All other options are handled by the Entry subwidget.

=back

=head1 DATE FORMATS

The default date format is MM/DD/YYYY. Since Tk::DateEntry has to parse the
date to decide which month to display, you can't specify strftime formats
directly (like "-dateformat => 'Date: %D. %B'").

The "builtin" date formats are:

=over 4

=item

-dateformat => 1       - MM/DD/YYYY (default)

=item

-dateformat => 2       - YYYY/MM/DD

=item

-dateformat => 3       - DD/MM/YYYY

=back

Trailing fields that are missing will be replaced by the current date, if the
year is specified by one or two digits, the widget will guess the century by
using a "100 year window".

If you're not satisified with any of these formats, you might specify your
own parse- and format routine by using the -parsecmd and -formatcmd options.

The -parsecmd subroutine will be called whenever the pulldown is opened.
The subroutine will be called with the current content of -textvariable as
the only argument. It should return a three element list: (year, month, day).
Any undefined elements will be replaced by default values.

The -formatcmd subroutine will be called whenever the user selects a date.
It will be called with three arguments: (year, month, day). It should return
a single string which will be assigned to the -textvariable.

See "EXAMPLES" below.

=head1 WEEKS

The default is to display the calendar the same way as the unix "cal" command
does: Weeks begin on Sunday, and the daynames are S, M, Tu, W, Th, F, and S.

However, some people prefer to start the weeks at Monday (saving both
Saturday and Sunday to the weekEND...)  This can be achived by specifying
-weekstart=>1. -weekstart=>0 causes the week to start at Sunday, which
is the default. If you have a very odd schedule, you could also start the
week at Wednesday by specifying -weekstart=>3 .....

If you don't like the "cal" headings, you might specify something else
by using the -daynames option.

See "EXAMPLES" below.

=head1 EXAMPLES

=head2 The simplest way:

	$parent-&>DateEntry-&>pack;

=head2 Other daynames:

If you want the "locale's abbreviated weekday name" you do it like this:

	use POSIX qw/strftime/;
	my @daynames=();
	foreach (0..6) {
		push @daynames,strftime("%a",0,0,0,1,1,1,$_);
	}
	$parent->DateEntry(-daynames=>\@daynames)->pack;

=head2 Other date formats:

A Norwegian would probably do something like this:

	my $dateentry=$parent->DateEntry
		(-weekstart=>1,
		 -daynames=>[qw/Son Man Tir Ons Tor Fre Lor/],
		 -parsecmd=>sub {
			my ($d,$m,$y) = ($_[0] =~ m/(\d*)\/(\d*)-(\d*)/);
			return ($y,$m,$d);
		 },
		 -formatcmd=>sub {
			sprintf ("%d/%d-%d",$_[2],$_[1],$_[0]);
		 }
		)->pack;

Note that this -parsecmd will return (undef,undef,undef) even if one or
two of the fields are present. A more sophisticated regex might be needed....

=head1 CAVEATS

Tk::DateEntry uses timelocal(), localtime() and strftime().
These functions are based on the standard unix time representation, which
is the number of seconds since 1/1/1970.

This means that Tk::DateEntry don't support dates prior to 1970, and on
a 32 bit computer it don't support dates after 12/31/2037.

=head1 SEE ALSO

Tk::Entry

=cut

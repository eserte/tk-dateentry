#!/usr/bin/perl 
#
#
BEGIN { $| = 1; print "1..2\n"; $^W = 1 }
END {print "not ok 1\n" unless $loaded;}
use Tk;
use Tk::DateEntry;
$loaded = 1;

if (!defined $ENV{BATCH}) { $ENV{BATCH} = 1 }

print "ok 1\n";

if ($ENV{LC_ALL} =~ /^de/ ||
    $ENV{LANG}   =~ /^de/) {
    @extra_args = (-weekstart => 1,
		   -daynames => [qw/So Mo Di Mi Do Fr Sa/],
		   -parsecmd => sub {
		       my($d,$m,$y) = ($_[0] =~ m/(\d*)\.(\d*)\.(\d*)/);
		       return ($y,$m,$d);
		   },
		   -formatcmd => sub {
		       sprintf ("%02d.%02d.%04d",$_[2],$_[1],$_[0]);
		   },
		  );
}

$mw=new MainWindow;

$arrowdownwin = $mw->Photo(-data => <<'EOF');
#define arrowdownwin2_width 9
#define arrowdownwin2_height 13
static char arrowdownwin2_bits[] = {
 0x00,0xfe,0x00,0xfe,0x00,0xfe,0x00,0xfe,0x00,0xfe,0x7c,0xfe,0x38,0xfe,0x10,
 0xfe,0x00,0xfe,0x00,0xfe,0x00,0xfe,0x00,0xfe,0x00,0xfe};
EOF

$mw->Label(-text=>'Select date:')->pack(-side=>'left');
my $de = $mw->DateEntry(-textvariable => \$date,
			-todaybackground => "green",
			-arrowimage => $arrowdownwin,
			@extra_args,
		       )->pack(-side=>'left');

if ($ENV{BATCH}) {
    $mw->update; $mw->after(200);
    # The used members are internals, do not use in regular programs!
    $mw->after(200, sub {
		   $de->{_backbutton}->invoke;
	       });
    $mw->after(700, sub {
		   $de->{_nextbutton}->invoke;
	       });
    $mw->after(1200, sub {
		   $de->{_daybutton}->[2]->[3]->invoke;
	       });
    $de->buttonDown;
    $mw->update;
    # This blocks until a date is clicked
    if (!defined $date || $date !~ /\d/) {
	print "not ";
    }
    print "ok 2\n";
} else {
    $mw->Button(-text => 'OK',
		-command => sub {
		    print "# Selected date: $date\n";
		    print "ok 2\n";
		    $mw->destroy;
		})->pack;
    MainLoop;
}


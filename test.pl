#!/usr/bin/perl 
#
#
BEGIN { $| = 1; print "1..2\n"; }
END {print "not ok 1\n" unless $loaded;}
use Tk;
use Tk::DateEntry;
$loaded = 1;

print "ok 1\n";

$mw=new MainWindow;

$mw->Label(-text=>'Select date:')->pack(-side=>'left');
$mw->DateEntry(-textvariable => \$date,
	       -todaybackground => "green",
	      )->pack(-side=>'left');

$mw->Button(-text => 'OK',
	    -command => sub {
		print "Selected date: $date\n";
		$mw->destroy;
	    })->pack;

MainLoop;

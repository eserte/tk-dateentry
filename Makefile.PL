# -*- perl -*-

use 5.005; # new "for" syntax

use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'Tk::DateEntry',
    'VERSION_FROM' => 'DateEntry.pm', # finds $VERSION
    'PREREQ_PM' => {Tk => 400},
    'dist'      => {'COMPRESS'=>'gzip -9f', 'SUFFIX' => '.gz',
                    'POSTOP'=>'-$(CHMOD) 644 $(DISTVNAME).tar$(SUFFIX)'},
);

sub MY::postamble {
    my $postamble = "";

    if (defined $ENV{USER} && $ENV{USER} eq 'eserte' && $^O =~ /bsd/i && -f "../../perl.release.mk") {
	$postamble .= <<'EOF';

.include "../../perl.release.mk"
.include "../../perl.cvs.mk"

EOF
    }

    $postamble;
}

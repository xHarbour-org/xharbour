#!/usr/bin/perl
#
# arm.pl   produce arminsa.c, arminsi.h, arminsn.c from arm.dat
#
# The Netwide Assembler is copyright (C) 1996 Simon Tatham and
# Julian Hall. All rights reserved. The software is
# redistributable under the licence given in the file "Licence"
# distributed in the NASM archive.
#
# Rehacked for ARM by Hans T Walheim Jan/2000 under the same license
#

print STDERR "Reading arm.dat...\n";

@args   = ();
undef $output;
foreach $arg ( @ARGV )
  {
    if ( $arg =~ /^\-/ )
      {
        if  ( $arg =~ /^\-([adin])$/ )
          {
            $output = $1;
            print "Option " .$1. " \n";
          }
        else
          {
            die "$0: Unknown option: ${arg}\n";
          }
      }
    else
      {
        push (@args, $arg);
      }
  }

$fname = "arm.dat" unless $fname = $args[0];
open (F, $fname) || die "unable to open $fname";

$line = 0;
$insns = 0;
while (<F>)
  {
    $line++;
    next if /^\s*;/;   # comments
        chomp;

    split;
    next if $#_ == -1; # blank lines
        (warn "line $line does not contain four fields\n"), next if $#_ != 3;
    ($formatted, $nd) = &format(@_);
    if ($formatted)
      {
        $insns++;
        $aname = "aa_$_[0]";
        push @$aname, $formatted;
      }
    if ( $_[0] =~ /cc$/ )
      {
        # Conditional instruction
        $k_opcodes_cc{$_[0]}++;
      }
    else
      {
        # Unconditional instruction
        $k_opcodes{$_[0]}++;
      }
    if ($formatted && !$nd)
      {
        push @big, $formatted;
        foreach $i (&startbyte($_[2]))
          {
            $aname = sprintf "dd_%02X",$i;
            push @$aname, $#big;
          }
      }
  }

close F;

@opcodes    = sort keys(%k_opcodes);
@opcodes_cc = sort keys(%k_opcodes_cc);

if ( !defined($output) || $output eq 'a' )
  {
    print STDERR "Writing arminsa.c...\n";

    open A, ">arminsa.c";

    print A "/* This file auto-generated from arm.dat by arm.pl" .
        " - don't edit it */\n\n";
    print A "/* This file is included by armemit.c */\n\n";

    foreach $i (@opcodes, @opcodes_cc)
      {
        print A "static struct ITEMPLATE instrux_${i}[] = {\n";
        $aname = "aa_$i";
        foreach $j (@$aname)
          {
            print A "    $j\n";
          }
        print A "    {-1}\n};\n\n";
      }
    print A "static struct ITEMPLATE *asm_instructions[] = {\n";
    foreach $i (@opcodes, @opcodes_cc)
      {
        print A "    instrux_${i},\n";
      }
    print A "};\n";

    close A;
  }

if ( !defined($output) || $output eq 'i' )
  {
    print STDERR "Writing arminsi.h...\n";

    open I, ">arminsi.h";

    print I "/* This file is auto-generated from arm.dat by arm.pl" .
        " - don't edit it */\n\n";
    print I "/* This file in included by asm.h */\n\n";

    print I "/* Instruction names */\n";
    print I "enum {";
    $first  = 1;
    $maxlen = 0;
    foreach $i (@opcodes, @opcodes_cc)
      {
        print I "," if ( !$first );
        $first = 0;
        print I "\n\tI_${i}";
        $len = length($i);
        $len++;                         # Can have terminating 'S'
#       $len++ if ( $i =~ /cc$/ );      # Condition codes are 2 characters long
        $maxlen = $len if ( $len > $maxlen );
      }
    print I "\n};\n\n";
    print I "#define MAX_ARMINSNLEN ", $maxlen, "\n";

    close I;
  }

if ( !defined($output) || $output eq 'n' )
  {
    print STDERR "Writing arminsn.c...\n";

    open N, ">arminsn.c";

    print N "/* This file is auto-generated from arm.dat by arm.pl" .
        " - don't edit it */\n\n";
    print N "/* This file in included by names.c */\n\n";

    print N "static const char *insn_names[] = {";
    $first = 1;
    foreach $i (@opcodes)
      {
        print N "," if ( !$first );
        $first = 0;
        $ilower = $i;
        $ilower =~ tr/A-Z/a-z/; # Change to lower case (Perl 4 compatible)
        print N "\n\t\"${ilower}\"";
      }
    print N "\n};\n\n";
    print N "/* Conditional instructions */\n";
    print N "static const char *cond_insn_names[] = {";
    $first = 1;
    foreach $i (@opcodes_cc)
      {
        print N "," if ( !$first );
        $first = 0;
        $ilower = $i;
        $ilower =~ s/cc$//;             # Skip cc suffix
        $ilower =~ tr/A-Z/a-z/; # Change to lower case (Perl 4 compatible)
        print N "\n\t\"${ilower}\"";
      }

    print N "\n};\n\n";
    print N "/* and the corresponding opcodes */\n";
    print N "static const int cond_insn_codes[] = {";
    $first = 1;
    foreach $i (@opcodes_cc)
      {
        print N "," if ( !$first );
        $first = 0;
        print N "\n\tI_$i";
      }

    print N "\n};\n";

    close N;
  }

printf STDERR "Done: %d instructions\n", $insns;

sub count_opers
  {
    local ($opers) = @_;
    local $num;

#    print "count: " .$opers. "\n";
    $num = scalar split (/,/,$opers);

#    print "split gave : ".$num. "\n";

    return ($num);
  }

sub format
  {
    local ($opcode, $operands, $codes, $flags) = @_;
    local $num, $nd = 0;

    local $tmp;

    return (undef, undef) if $operands eq "ignore";

# format the operands
    $operands =~ s/:/|a_colon,/g;
    $operands =~ s/mem(\d+)/mem|a_bits$1/g;
    $operands =~ s/mem/a_memory/g;
    $operands =~ s/a_memory_offs/a_mem_offs/g;
    $operands =~ s/imm(\d+)/imm|a_bits$1/g;
    $operands =~ s/imm/a_immediate/g;
    $operands =~ s/rm(\d+)/regmem|bits$1/g;
    # extensions by pelle (i totally *hate* perl)
    $operands =~ s/reg8/a_reg8/g;               # po 001113
    $operands =~ s/reg32/a_reg32/g;             # po 001113
    $operands =~ s/reglist/a_reglist/g;         # po 001113
    $operands =~ s/regf/a_regf/g;               # po 001113

    if ($operands eq 'void')
      {
        $operands = '0,0,0,0';
        $num = 0;
      }
    else
      {
        $tmp = count_opers ($operands);
        $num = $tmp;
        while ($tmp < 4)
          {
            $operands .= ',0';
            $tmp++;
          }
      }

    $operands =~ tr/a-z/A-Z/;

#    print "operands: ".$operands. "  num: " .$num. "\n";

# format the flags
    $flags =~ s/,/|IF_/g;
    $flags =~ s/(\|IF_ND|IF_ND\|)//, $nd = 1 if $flags =~ /IF_ND/;
            $flags = "IF_" . $flags;

  ("{I_$opcode, $num, {$operands}, \"$codes\", $flags},", $nd);
  }

# Here we determine the range of possible starting bytes for a given
# instruction. We need not consider anything for ARM:
sub startbyte
  {
    local ($codes) = @_;
    local $word, @range;

    while (1)
      {
        die "couldn't get code in '$codes'" if $codes !~ /^(\\[^\\]+)(\\.*)?$/;
        $word = $1, $codes = $2;

# short-circuit for ARM  Fix it proper later

        return (hex $1);

#       return (hex $1) if $word =~ /^\\[123]$/ && $codes =~ /^\\x(..)/;
#       return (0x07, 0x17, 0x1F) if $word eq "\\4";
#       return (0xA1, 0xA9) if $word eq "\\5";
#       return (0x06, 0x0E, 0x16, 0x1E) if $word eq "\\6";
#       return (0xA0, 0xA8) if $word eq "\\7";
#       $start=hex $1, $r=8, last if $word =~ /^\\1[012]$/ && $codes =~/^\\x(..)/;
#       return (0) if $word eq "\\17";
#       $start=hex $1, $r=16, last if $word =~ /^\\330$/ && $codes =~ /^\\x(..)/;
#       return () if $word eq "\\0" || $word eq "\\340";
      }
    @range = ();
    push @range, $start++ while ($r-- > 0);
    @range;
}

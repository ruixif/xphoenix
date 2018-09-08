*
**************************************************************
*
*  PHOENIX Version 2.1.4
*  Copyright (C) 2016 Wolfgang Sturhahn
*
*  This file is part of PHOENIX-2.1.4.
*
*  PHOENIX-2.1.4 is free software:
*  You can redistribute it and/or modify it under the terms
*  of the GNU General Public License as published by the
*  Free Software Foundation
*
*  PHOENIX-2.1.4 is distributed in the hope that it will be
*  useful, but WITHOUT ANY WARRANTY; without even the implied
*  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*  PURPOSE. See the GNU General Public License for details.
*
*  A copy of the GNU General Public License should have been
*  provided along with PHOENIX-2.1.4.
*  If not, see <http://www.gnu.org/licenses/>.
*
**************************************************************
*
      Subroutine PHXERR(LUNERR,NERR)
*
*
* Purpose :  analyse error situations, provide error
*            message printout, and take corrective
*            action if possible
* Date    :  November 7, 1997
* Author  :  Wolfgang Sturhahn
*
* import variables:
*    LUNERR     logical unit number used for printout
*                of error messages
*    NERR       error code created as created by
*                the subroutine
*                the following coding convention is used:
*                  ...xxnnn (each char. symbolizes a figure)
*                 xx  = codes the subroutine name
*                 nnn = error number
*
* export variables:
*    NERR       is set to zero if error recovery is possible
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<phox.h>
*
      CHARACTER*10 XSB(0:99)
*
      DATA XSB(0) / 'MAIN      ' /,
     +     XSB(1) / 'LESPHX    ' /,
     +     XSB(2) / 'LESDAT    ' /,
     +     XSB(3) / 'BGCORR    ' /,
     +     XSB(4) / 'LESXRF    ' /,
     +     XSB(5) / 'SAVDAT    ' /,
     +     XSB(9) / 'GFIT      ' /,
     +     XSB(10)/ 'TOPIPE    ' /
*
*:  derive subroutine name code, error code, and error level
*
      NAMEC = NERR/1000
      NERRL = MOD(NERR,1000)/100
      NERRC = MOD(NERR,100)
*
      WRITE(LUNERR,1) NERRL,XSB(NAMEC)
*
*:  error level 9 (fatal errors, no recovery)
*:  =========================================
*
      IF(NERRL.EQ.9) THEN
       BFOUND = .FALSE.
*
*:   main prog.
*
       IF(NAMEC.EQ.0) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG,' ')
         WRITE(LUNERR,901) XMSG(1:L),NCOM1
        ENDIF
       ENDIF
*
*:   subroutine LESPHX
*
       IF(NAMEC.EQ.1) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1901) NCOM1,NCOM2
        ENDIF
       ENDIF
*
*:   subr. LESDAT
*
       IF(NAMEC.EQ.2) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMES,' ')
         WRITE(LUNERR,2901) XMES(1:L),NCOM1
        ENDIF
       ENDIF
*
*:   subr. BGCORR
*
       IF(NAMEC.EQ.3) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,3901)
        ENDIF
       ENDIF
*
*:   subr. LESXRF
*
       IF(NAMEC.EQ.4) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,4901)
        ENDIF
       ENDIF
*
       IF(.NOT.BFOUND) WRITE(LUNERR,4)
       WRITE(LUNERR,2)
       RETURN
      ENDIF
*
*:  error level 8 (severe errors, no recovery)
*:  ==========================================
*
      IF(NERRL.EQ.8) THEN
       BFOUND = .FALSE.
*
*:   subroutine LESPHX
*
       IF(NAMEC.EQ.1) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1801)
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1802)
        ENDIF
*
        IF(NERRC.EQ.3) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1803)
        ENDIF
*
        IF(NERRC.EQ.4) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1804)
        ENDIF
*
        IF(NERRC.EQ.5) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1805)
        ENDIF
*
        IF(NERRC.EQ.6) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1806)
        ENDIF
*
        IF(NERRC.EQ.7) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1807)
        ENDIF
*
        IF(NERRC.EQ.8) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1808)
        ENDIF
*
        IF(NERRC.EQ.9) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1809)
        ENDIF
*
        IF(NERRC.EQ.10) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1810)
        ENDIF
*
        IF(NERRC.EQ.11) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1811)
        ENDIF
       ENDIF
*
*:   subr. LESDAT
*
       IF(NAMEC.EQ.2) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMES,' ')
         WRITE(LUNERR,2801) XMES(1:L),PCMAX
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L      = LENX(XMES,' ')
         WRITE(LUNERR,2802) XMES(1:L)
        ENDIF
*
        IF(NERRC.EQ.3) THEN
         BFOUND = .TRUE.
         L      = LENX(XMES,' ')
         WRITE(LUNERR,2803) XMES(1:L)
        ENDIF
       ENDIF
*
*:   subr. BGCORR
*
       IF(NAMEC.EQ.3) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,3801)
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,3802)
        ENDIF
       ENDIF
*
*:   subr. LESXRF
*
       IF(NAMEC.EQ.4) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMES,' ')
         WRITE(LUNERR,4801)
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG,' ')
         WRITE(LUNERR,4802) XMSG(1:L),PCMAX
        ENDIF
*
        IF(NERRC.EQ.3) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG,' ')
         WRITE(LUNERR,4803) XMSG(1:L)
        ENDIF
*
        IF(NERRC.EQ.4) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG,' ')
         WRITE(LUNERR,4804) XMSG(1:L)
        ENDIF
       ENDIF
*
*:   subr. GFIT
*
       IF(NAMEC.EQ.9) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMES,' ')
         WRITE(LUNERR,9801) XMES(1:L)
        ENDIF
       ENDIF
*
       IF(.NOT.BFOUND) WRITE(LUNERR,4)
       WRITE(LUNERR,2)
       RETURN
      ENDIF
*
*:  error level 7 (severe errors, recovery)
*:  =======================================
*
      IF(NERRL.EQ.7) THEN
       BFOUND = .FALSE.
*
*:   main prog.
*
       IF(NAMEC.EQ.0) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG,' ')
         WRITE(LUNERR,701) XMSG(1:L)
        ENDIF
       ENDIF
*
*:   subr. SAVDAT
*
       IF(NAMEC.EQ.5) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG,' ')
         WRITE(LUNERR,5701) XMSG,NCOM1
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG,' ')
         WRITE(LUNERR,5702) XMSG,NCOM1
        ENDIF
       ENDIF
*
*:   subr. TOPIPE
*
       IF(NAMEC.EQ.10) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L1     = LENX(XMSG,' ')
         L2     = LENX(XMSG1,' ')
         WRITE(LUNERR,10701) XMSG(1:L1),XMSG1(1:L2)
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG,' ')
         WRITE(LUNERR,10702) XMSG(1:L),NCOM1
        ENDIF
*
        IF(NERRC.EQ.3) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG,' ')
         WRITE(LUNERR,10703) XMSG(1:L)
        ENDIF
       ENDIF
*
       IF(BFOUND) THEN
        WRITE(LUNERR,3)
        NERR = 0
       ELSE
        WRITE(LUNERR,4)
        WRITE(LUNERR,2)
       ENDIF
       RETURN
      ENDIF
*
*:  error level 6 (not used)
*:  ========================
*
      IF(NERRL.EQ.6) THEN
       BFOUND = .FALSE.
*
       IF(BFOUND) THEN
        WRITE(LUNERR,3)
        NERR = 0
       ELSE
        WRITE(LUNERR,4)
        WRITE(LUNERR,2)
       ENDIF
       RETURN
      ENDIF
*
*:  error levels 5..1 (warnings, no recovery neccessary)
*:  ====================================================
*
      IF(NERRL.LE.5 .AND. NERRL.GE.1) THEN
       BFOUND = .FALSE.
*
       IF(BFOUND) THEN
        WRITE(LUNERR,3)
        NERR = 0
       ELSE
        WRITE(LUNERR,4)
        WRITE(LUNERR,2)
       ENDIF
       RETURN
      ENDIF
*
*:  error level 0 (information, no recovery neccessary)
*:  ===================================================
*
      IF(NERRL.EQ.0) THEN
       BFOUND = .FALSE.
*
       IF(.NOT.BFOUND) WRITE(LUNERR,4)
       WRITE(LUNERR,3)
       NERR = 0
       RETURN
      ENDIF
*
*:  error messages
*
    1 FORMAT(//'+++ error level',I2,' encountered'/
     +         '+++ caused by module ',A)
    2 FORMAT('+++ no corrective action taken'/
     +       '+++ program termination initiated'/)
    3 FORMAT('+++ corrective action taken'/
     +       '+++ program execution continues'/)
    4 FORMAT('+   unknown error')
*
*
  901 FORMAT(2X,'+ IO error opening file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file system')
*
  701 FORMAT(2X,'+ central peak is too large by ',A,' %'/
     +       2X,'+ results are probably meaningless')
*
*
 1901 FORMAT(2X,'+ IO error reading the standard input file'/
     +       2X,'+ system IO error',I5,' in line',I5/
     +       2X,'+ recommended action: check file')
*
 1801 FORMAT(2X,'+ standard input file: too few input lines')
*
 1802 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: transition energy'/
     +       2X,'+ recommended action: specify positive value')
*
 1803 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: recoil energy'/
     +       2X,'+ recommended action: specify positive value')
*
 1804 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: sample temperature'/
     +       2X,'+ recommended action: specify positive value')
*
 1805 FORMAT(2X,'+ standard input file: parameter missing'/
     +       2X,'+ parameter: prefix of output files'/
     +       2X,'+ recommended action: specify prefix')
*
 1806 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: fit range'/
     +       2X,'+ recommended action: specify positive value')
*
 1807 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: convolution range'/
     +       2X,'+ recommended action: specify positive value')
*
 1808 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: left/right asymmetry'/
     +       2X,'+ recommended action: specify value btw 0.1 and 10')
*
 1809 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: FT spectrum fraction'/
     +       2X,'+ recommended action: specify value btw 0.01 and 2')
*
 1810 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: filter power coefficient'/
     +       2X,'+ recommended action: specify positive integer')
*
 1811 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: FWHM of smoothing function'/
     +       2X,'+ recommended action: specify positive value')
*
*
 2901 FORMAT(2X,'+ IO error opening file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file name')
*
 2801 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column number >',I3/
     +       2X,'+ recommended action: select proper column numbers')
*
 2802 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column number < 1'/
     +       2X,'+ recommended action: specify positive value')
*
 2803 FORMAT(2X,'+ input file "',A,'": too few data points'/
     +       2X,'+ recommended action: check input file')
*
*
 3901 FORMAT(2X,'+ standard input file: read error'/
     +       2X,'+ parameter: while evaluating line 5'/
     +       2X,'+ recommended action: check syntax')
*
 3801 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: nr. of points to average'/
     +       2X,'+ recommended action: specify positive value')
*
 3802 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: constant background'/
     +       2X,'+ recommended action: specify positive value')
*
*
 4901 FORMAT(2X,'+ IO error opening file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file name')
*
 4801 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: shape of resolution function'/
     +       2X,'+ recommended action: specify value btw 0.1 and 10')
*
 4802 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column number >',I3/
     +       2X,'+ recommended action: select proper column numbers')
*
 4803 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column number < 1'/
     +       2X,'+ recommended action: specify positive value')
*
 4804 FORMAT(2X,'+ input file "',A,'": too few data points'/
     +       2X,'+ recommended action: check input file')
*
*
 5701 FORMAT(2X,'+ IO error opening file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file system')
*
 5702 FORMAT(2X,'+ IO error writing to file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file system')
*
*
 9801 FORMAT(2X,'+ input file "',A,
     +               '": too few data points in fit range'/
     +       2X,'+ recommended action: modify fit range')
*
*
10701 FORMAT(2X,'+ cannot reconcile "',A,'"'/
     +       2X,'+ with visualization options "',A,'"'/
     +       2X,'+ action: visualization is disabled')
*
10702 FORMAT(2X,'+ IO error opening pipe "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ action: visualization is disabled')
*
10703 FORMAT(2X,'+ IO error writing to pipe "',A,'"'/
     +       2X,'+ action: visualization is disabled')
*
*
      END

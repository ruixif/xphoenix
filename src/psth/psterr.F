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
      SUBROUTINE PSTERR(LUNERR,NERR)
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
#include<psth.h>
*
      CHARACTER*10 XSB(0:99)
*
      DATA XSB(0) / 'MAIN      ' /,
     +     XSB(1) / 'LESPST    ' /,
     +     XSB(2) / 'LESDOS    ' /,
     +     XSB(3) / 'SOFE      ' /,
     +     XSB(4) / 'SAVDAT    ' /,
     +     XSB(5) / '          ' /,
     +     XSB(9) / '          ' /,
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
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,901) XMSG1(1:L),NCOM1
        ENDIF
       ENDIF
*
*:   subroutine LESPST
*
       IF(NAMEC.EQ.1) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1901) NCOM1,NCOM2
        ENDIF
       ENDIF
*
*:   subroutine LESDOS
*
       IF(NAMEC.EQ.2) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XDOS,' ')
         WRITE(LUNERR,2901) XDOS(1:L),NCOM1
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
*:   subroutine LESPST
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
       ENDIF
*
*:   subr. LESDOS
*
       IF(NAMEC.EQ.2) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XDOS,' '))
         WRITE(LUNERR,2801) XDOS(1:L),PCMAX
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XDOS,' '))
         WRITE(LUNERR,2802) XDOS(1:L)
        ENDIF
       ENDIF
*
*:   subr. SAVDAT
*
       IF(NAMEC.EQ.4) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,4801) XMSG1(1:L),NCOM1
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,4802) XMSG1(1:L),NCOM1
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
*:   subr. SOFE
*
       IF(NAMEC.EQ.3) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,3701) DENG1,DENG2,NENG
        ENDIF
       ENDIF
*
*:   subr. TOPIPE
*
       IF(NAMEC.EQ.10) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L1     = MAX0(1,LENX(XMSG1,' '))
         L2     = MAX0(1,LENX(XMSG2,' '))
         WRITE(LUNERR,10701) XMSG1(1:L1),XMSG2(1:L2)
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMSG1,' '))
         WRITE(LUNERR,10702) XMSG1(1:L),NCOM1
        ENDIF
*
        IF(NERRC.EQ.3) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMSG1,' '))
         WRITE(LUNERR,10703) XMSG1(1:L)
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
*
 1901 FORMAT(2X,'+ IO error reading the standard input file'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ error occurred in line',I5/
     +       2X,'+ recommended action: check file')
*
 1801 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: transition energy'/
     +       2X,'+ recommended action: specify positive value')
*
 1802 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: recoil energy'/
     +       2X,'+ recommended action: specify positive value')
*
 1803 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: sample temperature'/
     +       2X,'+ recommended action: specify positive value(s)')
*
 1804 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: number of temperature points'/
     +       2X,'+ recommended action: specify positive value')
*
 1805 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: number of energy points'/
     +       2X,'+ recommended action: specify positive value')
*
 1806 FORMAT(2X,'+ standard input file: parameter missing'/
     +       2X,'+ parameter: prefix of output files'/
     +       2X,'+ recommended action: specify prefix')
*
 1807 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: shape coefficient'/
     +       2X,'+ recommended action: specify larger positive value')
*
 1808 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: asymmetry coefficient'/
     +       2X,'+ recommended action: specify larger positive value')
*
*
 2901 FORMAT(2X,'+ IO error opening file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file name')
*
 2801 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column number >',I3/
     +       2X,'+ recommended action: inspect file')
*
 2802 FORMAT(2X,'+ input file "',A,'": less than 11 data points'/
     +       2X,'+ recommended action: inspect file content')
*
*
 3701 FORMAT(2X,'+ standard input file: parameter(s) out of range'/
     +       2X,'+ parameter(s): energy range for output'/
     +       2X,'+ action: new range is',F8.2,' ->',F8.2,' meV;',
     +                 I5,' points')
*
*
 4801 FORMAT(2X,'+ IO error opening file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file system')
*
 4802 FORMAT(2X,'+ IO error writing to file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file system')
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

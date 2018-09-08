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
      SUBROUTINE PSVERR(LUNERR,NERR)
*
*
* Purpose :  analyse error situations, provide error
*            message printout, and take corrective
*            action if possible
* Date    :  November 10, 2006
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
#include<psvl.h>
*
      CHARACTER*10 XSB(0:99)
*
      DATA XSB(0) / 'MAIN      ' /,
     +     XSB(1) / 'LESPSV    ' /,
     +     XSB(2) / 'LESDAT    ' /,
     +     XSB(3) / 'SAVDAT    ' /,
     +     XSB(4) / 'TOPIPE    ' /
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
*:   main
*
       IF(NAMEC.EQ.0) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L = LENX(XMSG,' ')
         WRITE(LUNERR,901) XMSG(1:L),MSG1
        ENDIF
       ENDIF
*
*:   subroutine LESPSV
*
       IF(NAMEC.EQ.1) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1901) MSG1,MSG2
        ENDIF
       ENDIF
*
*:   subroutine LESDAT
*
       IF(NAMEC.EQ.2) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG,' ')
         WRITE(LUNERR,2901) XMSG(1:L),MSG1
        ENDIF
       ENDIF
*
*:   subroutine SAVDAT
*
       IF(NAMEC.EQ.3) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG,' ')
         WRITE(LUNERR,3901) XMSG(1:L),MSG1
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
*:   subroutine LESPSV
*
       IF(NAMEC.EQ.1) THEN
*
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
*:   subr. LESPSV
*
       IF(NAMEC.EQ.1) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1701)
        ENDIF
       ENDIF
*
*:   subr. TOPIPE
*
       IF(NAMEC.EQ.4) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG,' ')
         L1     = LENX(XMSG1,' ')
         WRITE(LUNERR,4701) XMSG(1:L),XMSG1(1:L1)
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG,' ')
         WRITE(LUNERR,4702) XMSG(1:L),MSG1
        ENDIF
*
        IF(NERRC.EQ.3) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG,' ')
         WRITE(LUNERR,4703) XMSG(1:L)
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
     +       2X,'+ recommended action: check file name')
*
*
 1901 FORMAT(2X,'+ IO error reading the standard input file'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ error occurred in line',I5/
     +       2X,'+ recommended action: check file')
*
 1801 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: mass number of resonant isotope'/
     +       2X,'+ recommended action: specify positive value')
*
 1802 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: pressure'/
     +       2X,'+ recommended action: specify non-negative value')
*
 1803 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: density and/or its error'/
     +       2X,'+ recommended action: specify positive value')
*
 1804 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: bulk modulus and/or its error'/
     +       2X,'+ recommended action: specify positive value')
*
 1805 FORMAT(2X,'+ standard input file: parameter out of range'/
     + 2X,'+ parameter: derivative of bulk modulus and/or its error'/
     +       2X,'+ recommended action: specify positive value')
*
 1701 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: EOS type'/
     +       2X,'+ action: EOS type set to Birch-Murnaghan')
*
*
 2901 FORMAT(2X,'+ IO error opening file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file name')
*
 2801 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column number'/
     +       2X,'+ recommended action: min.value=1 max.value=',I2)
*
 2802 FORMAT(2X,'+ input file "',A,'": too few data points'/
     +       2X,'+ recommended action: check file')
*
*
 3901 FORMAT(2X,'+ IO error opening file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file system')
*
*
 4701 FORMAT(2X,'+ cannot reconcile "',A,'"'/
     +       2X,'+ with visualization options "',A,'"'/
     +       2X,'+ action: visualization is disabled')
*
 4702 FORMAT(2X,'+ IO error opening pipe "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ action: visualization is disabled')
*
 4703 FORMAT(2X,'+ IO error writing to pipe "',A,'"'/
     +       2X,'+ action: visualization is disabled')
*
*
      END

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
      Subroutine PADERR(LUNERR,NERR)
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
#include<padd.h>
*
      CHARACTER*10 XSB(0:99)
*
      DATA XSB(0) / 'MAIN      ' /,
     +     XSB(1) / 'LESPAD    ' /,
     +     XSB(2) / 'FMTRD     ' /,
     +     XSB(3) / 'API       ' /,
     +     XSB(4) / 'LESXRF    ' /,
     +     XSB(5) / 'SAVSHF    ' /,
     +     XSB(6) / 'SAVSUM    ' /,
     +     XSB(7) / 'SAVNRM    ' /,
     +     XSB(8) / 'DTCORR    ' /,
     +     XSB(9) / 'GFIT      ' /,
     +     XSB(10)/ 'SAVMON    ' /,
     +     XSB(11)/ 'ANGCAL    ' /,
     +     XSB(12)/ '          ' /,
     +     XSB(13)/ 'TOPIPE    ' /
*
*:  derive subroutine name code, error code, and error level
*
      NAMEC = NERR/1000
      NERRL = Mod(NERR,1000)/100
      NERRC = Mod(NERR,100)
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
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,901) XMSG1(1:L),NCOM1
        ENDIF
       ENDIF
*
*:   subroutine LESPAD
*
       IF(NAMEC.EQ.1) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,1901) NCOM1,NCOM2
        ENDIF
       ENDIF
*
*:   subroutine LESXRF
*
       IF(NAMEC.EQ.4) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,4901) XMSG1(1:L),NCOM1
        ENDIF
       ENDIF
*
*:   subroutine SAVSUM
*
       IF(NAMEC.EQ.6) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,6901) XMSG1(1:L),NCOM1
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,6902) XMSG1(1:L),NCOM1
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
*:   main prog.
*
       IF(NAMEC.EQ.0) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,801) XMSG1(1:L)
        ENDIF
       ENDIF
*
*:   subroutine LESPAD
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
         L1     = Lenx(XPLGIN,' ')
         L2     = Lenx(XDIR,' ')
         WRITE(LUNERR,1806) XPLGIN(1:L1),XDIR(1:L2)
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
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,4802) XMSG1(1:L),PCMAX
        ENDIF
*
        IF(NERRC.EQ.3) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,4803) XMSG1(1:L)
        ENDIF
*
        IF(NERRC.EQ.4) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,4804) XMSG1(1:L)
        ENDIF
       ENDIF
*
*:   subroutine DTCORR
*
       IF(NAMEC.EQ.8) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,8801)
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         WRITE(LUNERR,8802)
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
*:   subr. FMTRD
*
       IF(NAMEC.EQ.2) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,2701) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,2702) XMES(NCOM1)(1:L)
        ENDIF
*
        IF(NERRC.EQ.3) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,2703) XMES(NCOM1)(1:L)
        ENDIF
*
        IF(NERRC.EQ.4) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,2704) XMES(NCOM1)(1:L)
        ENDIF
       ENDIF
*
*:   subr. API
*
       IF(NAMEC.EQ.3) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3701) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3702) XMES(NCOM1)(1:L)
        ENDIF
*
        IF(NERRC.EQ.3) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3703) XMES(NCOM1)(1:L)
        ENDIF
*
        IF(NERRC.EQ.4) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3704) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.5) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3705) XMES(NCOM1)(1:L),NCOM2
        ENDIF
       ENDIF
*
*:   subr. SAVSHF
*
       IF(NAMEC.EQ.5) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,5701) XMSG1(1:L),NCOM1
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,5702) XMSG1(1:L),NCOM1
        ENDIF
       ENDIF
*
*:   subr. SAVNRM
*
       IF(NAMEC.EQ.7) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,7701) XMSG1(1:L),NCOM1
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,7702) XMSG1(1:L),NCOM1
        ENDIF
       ENDIF
*
*:   subr. GFIT
*
       IF(NAMEC.EQ.9) THEN
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMES(NCOM1),' ')
         WRITE(LUNERR,9701) XMES(NCOM1)(1:L)
        ENDIF
       ENDIF
*
*:   subr. SAVMON
*
       IF(NAMEC.EQ.10) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,10701) XMSG1(1:L),NCOM1
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,10702) XMSG1(1:L),NCOM1
        ENDIF
       ENDIF
*
*:   subroutine ANGCAL
*
       IF(NAMEC.EQ.11) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L1 = LENX(XMSG1,' ')
         L2 = LENX(XMES(NCOM1),' ')
         WRITE(LUNERR,11701) XMES(NCOM1)(1:L1),XMSG1(1:L2)
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L1 = LENX(XMSG1,' ')
         L2 = LENX(XMES(NCOM1),' ')
         WRITE(LUNERR,11702) XMES(NCOM1)(1:L1),XMSG1(1:L2)
        ENDIF
       ENDIF
*
*:   subr. TOPIPE
*
       IF(NAMEC.EQ.13) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L1     = LENX(XMSG1,' ')
         L2     = LENX(XMSG2,' ')
         WRITE(LUNERR,13701) XMSG1(1:L1),XMSG2(1:L2)
        ENDIF
*
        IF(NERRC.EQ.2) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,13702) XMSG1(1:L),NCOM1
        ENDIF
*
        IF(NERRC.EQ.3) THEN
         BFOUND = .TRUE.
         L      = LENX(XMSG1,' ')
         WRITE(LUNERR,13703) XMSG1(1:L)
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
      IF(NERRL.EQ.5) THEN
       BFOUND = .FALSE.
*
*:   subr. FMTRD
*
       IF(NAMEC.EQ.2) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,2501) XMES(NCOM1)(1:L)
        ENDIF
*
        IF(NERRC.EQ.3) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,2503) XMES(NCOM1)(1:L)
        ENDIF
*
        IF(NERRC.EQ.4) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,2504) XMES(NCOM1)(1:L)
        ENDIF
*
        IF(NERRC.EQ.5) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,2505) XMES(NCOM1)(1:L)
        ENDIF
*
        IF(NERRC.EQ.6) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,2506) XMES(NCOM1)(1:L)
        ENDIF
*
        IF(NERRC.EQ.7) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,2507) XMES(NCOM1)(1:L)
        ENDIF
*
        IF(NERRC.EQ.8) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,2508) XMES(NCOM1)(1:L)
        ENDIF
*
        IF(NERRC.EQ.9) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,2509) XMES(NCOM1)(1:L)
        ENDIF
*
        IF(NERRC.EQ.10) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,2510) XMES(NCOM1)(1:L)
        ENDIF
*
        IF(NERRC.EQ.11) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,2511) XMES(NCOM1)(1:L)
        ENDIF
*
        IF(NERRC.EQ.12) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,2512) XMES(NCOM1)(1:L)
        ENDIF
       ENDIF
*
*:   subr. API
*
       IF(NAMEC.EQ.3) THEN
*
        IF(NERRC.EQ.1) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3501) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.3) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3503) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.4) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3504) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.5) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3505) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.6) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3506) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.7) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3507) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.8) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3508) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.9) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3509) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.10) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3510) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.11) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3511) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.12) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3512) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.13) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3513) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.15) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3515) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.16) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3516) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.17) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3517) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.18) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3518) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.19) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3519) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.20) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3520) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.21) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3521) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.22) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3522) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.23) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3523) XMES(NCOM1)(1:L),NCOM2
        ENDIF
*
        IF(NERRC.EQ.24) THEN
         BFOUND = .TRUE.
         L      = MAX0(1,LENX(XMES(NCOM1),' '))
         WRITE(LUNERR,3524) XMES(NCOM1)(1:L),NCOM2
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
*
      IF(NERRL.LT.5 .AND. NERRL.GE.1) THEN
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
  801 FORMAT(2X,'+ input file "',A,'": processing issue'/
     +       2X,'+ issue: monitor policy changed'/
     +       2X,'+ recommended action: define monitor for all files')
*
*
 1901 FORMAT(2X,'+ IO error reading the standard input file'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ error occurred in line',I5/
     +       2X,'+ recommended action: check file')
*
 1801 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: operating energy, field (1)'/
     +       2X,'+ recommended action: specify positive value')
*
 1802 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: sample temperature, field (2)'/
     +       2X,'+ recommended action: specify positive value')
*
 1803 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: lattice constant, field (3)'/
     +       2X,'+ recommended action: specify positive value')
*
 1804 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: scaling factor, field (7)'/
     +       2X,'+ recommended action: specify nonzero value')
*
 1805 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: left/right asymmetry, field (12)'/
     +       2X,'+ recommended action: specify value within [0.1,10]')
*
 1806 FORMAT(2X,'+ standard input file: invalid parameter'/
     +       2X,'+ parameter: plugin ''',A,''' not found in ''',A,''''/
     +       2X,'+ recommended action: check parameter')
*
*
 2701 FORMAT(2X,'+ IO error opening file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ action: file is ignored')
*
 2702 FORMAT(2X,'+ input file "',A,'": missing parameter'/
     +       2X,'+ parameter: column definition ''D'''/
     +       2X,'+ action: file is ignored')
*
 2703 FORMAT(2X,'+ input file "',A,'": missing parameter'/
     +       2X,'+ parameter: column definition ''E'' and ''A'''/
     +       2X,'+ action: file is ignored')
*
 2704 FORMAT(2X,'+ input file "',A,'": too few data points'/
     +       2X,'+ action: file is ignored')
*
 2501 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column definition ''E'''/
     +       2X,'+ action: column definition is ignored')
*
 2503 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column definition ''a'''/
     +       2X,'+ action: column definition is ignored')
*
 2504 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column definition ''A'''/
     +       2X,'+ action: column definition is ignored')
*
 2505 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column definition ''c'''/
     +       2X,'+ action: column definition is ignored')
*
 2506 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column definition ''C'''/
     +       2X,'+ action: column definition is ignored')
*
 2507 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column definition ''t'''/
     +       2X,'+ action: column definition is ignored')
*
 2508 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column definition ''T'''/
     +       2X,'+ action: column definition is ignored')
*
 2509 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column definition ''S'''/
     +       2X,'+ action: column definition is ignored')
*
 2510 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column definition ''N'''/
     +       2X,'+ action: column definition is ignored')
*
 2511 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column definition ''M'''/
     +       2X,'+ action: column definition is ignored')
*
 2512 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: column definition ''P'''/
     +       2X,'+ action: column definition is ignored')
*
*
 3701 FORMAT(2X,'+ input file "',A,'", scan',I4,': cannot convert'/
     +       2X,'+ action: file is ignored')
*
 3702 FORMAT(2X,'+ input file "',A,'", scan',I4,': invalid parameter'/
     +       2X,'+ parameter: column definition ''D'''/
     +       2X,'+ action: file is ignored')
*
 3703 FORMAT(2X,'+ input file "',A,'", scan',I4,': invalid parameter'/
     +       2X,'+ parameter: column definition ''E'' and ''A'''/
     +       2X,'+ action: file is ignored')
*
 3704 FORMAT(2X,'+ input file "',A,'", scan',I4,': conversion problem'/
     +       2X,'+ parameter: column definition ''D'''/
     +       2X,'+ action: file is ignored')
*
 3705 FORMAT(2X,'+ input file "',A,'", scan',I4,': too few data points'/
     +       2X,'+ action: file is ignored')
*
 3501 FORMAT(2X,'+ input file "',A,'", scan',I4,': invalid parameter'/
     +       2X,'+ parameter: column definition ''E'''/
     +       2X,'+ action: column definition is ignored')
*
 3503 FORMAT(2X,'+ input file "',A,'", scan',I4,': invalid parameter'/
     +       2X,'+ parameter: column definition ''a'''/
     +       2X,'+ action: column definition is ignored')
*
 3504 FORMAT(2X,'+ input file "',A,'", scan',I4,': invalid parameter'/
     +       2X,'+ parameter: column definition ''A'''/
     +       2X,'+ action: column definition is ignored')
*
 3505 FORMAT(2X,'+ input file "',A,'", scan',I4,': invalid parameter'/
     +       2X,'+ parameter: column definition ''c'''/
     +       2X,'+ action: column definition is ignored')
*
 3506 FORMAT(2X,'+ input file "',A,'", scan',I4,': invalid parameter'/
     +       2X,'+ parameter: column definition ''C'''/
     +       2X,'+ action: column definition is ignored')
*
 3507 FORMAT(2X,'+ input file "',A,'", scan',I4,': invalid parameter'/
     +       2X,'+ parameter: column definition ''t'''/
     +       2X,'+ action: column definition is ignored')
*
 3508 FORMAT(2X,'+ input file "',A,'", scan',I4,': invalid parameter'/
     +       2X,'+ parameter: column definition ''T'''/
     +       2X,'+ action: column definition is ignored')
*
 3509 FORMAT(2X,'+ input file "',A,'", scan',I4,': invalid parameter'/
     +       2X,'+ parameter: column definition ''S'''/
     +       2X,'+ action: column definition is ignored')
*
 3510 FORMAT(2X,'+ input file "',A,'", scan',I4,': invalid parameter'/
     +       2X,'+ parameter: column definition ''N'''/
     +       2X,'+ action: column definition is ignored')
*
 3511 FORMAT(2X,'+ input file "',A,'", scan',I4,': invalid parameter'/
     +       2X,'+ parameter: column definition ''M'''/
     +       2X,'+ action: column definition is ignored')
*
 3512 FORMAT(2X,'+ input file "',A,'", scan',I4,': invalid parameter'/
     +       2X,'+ parameter: column definition ''P'''/
     +       2X,'+ action: column definition is ignored')
*
 3513 FORMAT(2X,'+ input file "',A,'", scan',I4,': conversion problem'/
     +       2X,'+ parameter: column definition ''E'''/
     +       2X,'+ action: column definition is ignored')
*
 3515 FORMAT(2X,'+ input file "',A,'", scan',I4,': conversion problem'/
     +       2X,'+ parameter: column definition ''a'''/
     +       2X,'+ action: column definition is ignored')
*
 3516 FORMAT(2X,'+ input file "',A,'", scan',I4,': conversion problem'/
     +       2X,'+ parameter: column definition ''A'''/
     +       2X,'+ action: column definition is ignored')
*
 3517 FORMAT(2X,'+ input file "',A,'", scan',I4,': conversion problem'/
     +       2X,'+ parameter: column definition ''c'''/
     +       2X,'+ action: column definition is ignored')
*
 3518 FORMAT(2X,'+ input file "',A,'", scan',I4,': conversion problem'/
     +       2X,'+ parameter: column definition ''C'''/
     +       2X,'+ action: column definition is ignored')
*
 3519 FORMAT(2X,'+ input file "',A,'", scan',I4,': conversion problem'/
     +       2X,'+ parameter: column definition ''t'''/
     +       2X,'+ action: column definition is ignored')
*
 3520 FORMAT(2X,'+ input file "',A,'", scan',I4,': conversion problem'/
     +       2X,'+ parameter: column definition ''T'''/
     +       2X,'+ action: column definition is ignored')
*
 3521 FORMAT(2X,'+ input file "',A,'", scan',I4,': conversion problem'/
     +       2X,'+ parameter: column definition ''S'''/
     +       2X,'+ action: column definition is ignored')
*
 3522 FORMAT(2X,'+ input file "',A,'", scan',I4,': conversion problem'/
     +       2X,'+ parameter: column definition ''N'''/
     +       2X,'+ action: column definition is ignored')
*
 3523 FORMAT(2X,'+ input file "',A,'", scan',I4,': conversion problem'/
     +       2X,'+ parameter: column definition ''M'''/
     +       2X,'+ action: column definition is ignored')
*
 3524 FORMAT(2X,'+ input file "',A,'", scan',I4,': conversion problem'/
     +       2X,'+ parameter: column definition ''P'''/
     +       2X,'+ action: column definition is ignored')
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
 6901 FORMAT(2X,'+ IO error opening file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file system')
*
 6902 FORMAT(2X,'+ IO error writing to file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file system')
*
*
 7701 FORMAT(2X,'+ IO error opening file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file system')
*
 7702 FORMAT(2X,'+ IO error writing to file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file system')
*
*
 8801 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: miller indices of 1.reflection'/
     +       2X,'+ recommended action: check parameter')
*
 8802 FORMAT(2X,'+ standard input file: parameter out of range'/
     +       2X,'+ parameter: miller indices of 2.reflection'/
     +       2X,'+ recommended action: check parameter')
*
*
 9701 FORMAT(2X,'+ input file "',A,
     +               '": too few data points in fit range'/
     +       2X,'+ action: file is ignored')
*
*
10701 FORMAT(2X,'+ IO error opening file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file system')
*
10702 FORMAT(2X,'+ IO error writing to file "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check file system')
*
*
11701 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: angle calibration of 1.refl. = ',A/
     +       2X,'+ recommended action: maintain 0.9 < value < 1.1')
*
11702 FORMAT(2X,'+ input file "',A,'": parameter out of range'/
     +       2X,'+ parameter: angle calibration of 2.refl. = ',A/
     +       2X,'+ recommended action: maintain 0.9 < value < 1.1')
*
*
13701 FORMAT(2X,'+ cannot reconcile "',A,'"'/
     +       2X,'+ with visualization options "',A,'"'/
     +       2X,'+ action: visualization is disabled')
*
13702 FORMAT(2X,'+ IO error opening pipe "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ action: visualization is disabled')
*
13703 FORMAT(2X,'+ IO error writing to pipe "',A,'"'/
     +       2X,'+ action: visualization is disabled')
*
*
      END

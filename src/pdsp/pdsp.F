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
*   Program PHOENIX module PDSP
*
* purpose :  Visialization of PHOENIX results
* date    :  August 14, 2009
* author  :  Wolfgang Sturhahn
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
* 
#include<phoenix.h>
*
      Character*4   XTAG
      Character*20  XVERSION
      Character*72  XPIPE,XH
      Character*200 XDIR
*
      Dimension RSX(PDATA),RSY(PDATA),RSDY(PDATA),RTY(PDATA),RT(2)
*
*
*:  set version number
*
#include<version.h>
*
*:  start global timer
*
      Call TIMEC(RT,1,1,-1)
*
*:  print copyright message
*
      Call COPYRD(6,XVERSION)
*
*:  obtain line arguments
*
      Call INSOPT(BRUSE,BGRAPH,BPRT,XDIR,XPIPE)
*
*:  get size of window
*
      Call GETENV('WINDOW_SIZE_X',XH)
      Read(XH,'(I5)') ISX
      ISX = Max(200,ISX)
*
      Call GETENV('WINDOW_SIZE_Y',XH)
      Read(XH,'(I5)') ISY
      ISY = Max(200,ISY)
*
*:  get desired position for window
*
      Call GETENV('WINDOW_POSITION_X',XH)
      Read(XH,'(I5)') IWPX
      IWPX = Max(0,IWPX)
*
      Call GETENV('WINDOW_POSITION_Y',XH)
      Read(XH,'(I5)') IWPY
      IWPY = Max(0,IWPY)
*
*:  open pipe for input
*
      Open(UNIT=LUNINP,FILE=XPIPE,STATUS='old',
     +        FORM='formatted',IOSTAT=IERR,ERR=998)
*
      Write(6,2000) XPIPE(1:LENX(XPIPE,' '))
 2000 Format(1X,'-- Listening to pipe'/5X,'''',A,''''/
     +       1X,'-- Ctrl-C to exit ...',$)
*
*:  start reading the pipe
*
      KX  = 0
      KY  = 0
      KDY = 0
*
      BPSVL = .false.
      BPSTH = .false.
      BPHOX = .false.
      BPADD = .false.
*
  200 Continue
       Read(LUNINP,'(A)',ERR=997) XTAG
       Call XTOUP(XTAG)
*
*:  analyze tag
*
       If(XTAG.eq.'EXIT') Then
        Close(UNIT=LUNPIP)
        GoTo 999
       Endif
*
       If(XTAG.eq.'PSVL') Then
        BPSVL = .TRUE.
        BPADD = .FALSE.
        BPHOX = .FALSE.
        BPSTH = .FALSE.
        GoTo 200
       Endif
*
       If(XTAG.eq.'PSTH') Then
        BPSTH = .True.
        BPADD = .FALSE.
        BPHOX = .FALSE.
        BPSVL = .FALSE.
        GoTo 200
       Endif
*
       If(XTAG.eq.'PHOX') Then
        BPHOX = .TRUE.
        BPADD = .FALSE.
        BPSTH = .FALSE.
        BPSVL = .FALSE.
        GoTo 200
       Endif
*
       If(XTAG.eq.'PADD') Then
        BPADD = .TRUE.
        BPHOX = .FALSE.
        BPSTH = .FALSE.
        BPSVL = .FALSE.
        GoTo 200
       Endif
*
       If(XTAG.eq.'SHOW') Then
        Read(LUNINP,*) NCALL
*
        If(BPADD) Then
         KS = Min(KX,KY,KDY)
         Call DSP_PADD(BRUSE,RSX,RSY,RSDY,KS,KT1,PDATA,
     +                  ISX,ISY,IWPX,IWPY,'Phoenix-padd',XDIR)
         GoTo 200
        Endif
*
        If(BPHOX) Then
         KS = Min(KX,KY,KDY)
         Call DSP_PHOX(NCALL,BRUSE,RSX,RSY,RSDY,KS,PDATA,
     +                  ISX,ISY,IWPX,IWPY,'Phoenix-phox',XDIR)
         GoTo 200
        Endif
*
        If(BPSTH) Then
         If(NCALL.eq.1) Then
          KT2 = KX
          KT1 = 1000
         Endif
         Call DSP_PSTH(NCALL,BRUSE,RSX,RSY,RTY,KT2,KT1,PDATA,
     +                  ISX,ISY,IWPX,IWPY,'Phoenix-psth',XDIR)
         GoTo 200
        Endif
*
        If(BPSVL) Then
         KS = Min(KX,KY,KDY)
         Call DSP_PSVL(NCALL,BRUSE,RSX,RSY,RSDY,RTY,KS,KT1,KT2,KT3,
     +                  PDATA,ISX,ISY,IWPX,IWPY,'Phoenix-psvl',XDIR)
         GoTo 200
        Endif
       Endif
*
       If(XTAG.eq.'X---') Then
        Read(LUNINP,*) KX
        Do 1  K = 1,KX
         Read(LUNINP,*) RSX(K)
    1   Continue
        GoTo 200
       Endif
*
       If(XTAG.eq.'Y---') Then
        Read(LUNINP,*) KY
        Do 2  K = 1,KY
         Read(LUNINP,*) RSY(K)
    2   Continue
        GoTo 200
       Endif
*
       If(XTAG.eq.'DY--') Then
        Read(LUNINP,*) KDY
        Do 3  K = 1,KDY
         Read(LUNINP,*) RSDY(K)
    3   Continue
        GoTo 200
       Endif
*
       If(XTAG.eq.'T---') Then
        Read(LUNINP,*) KT2,KT1,KT3
        Do 4  K = 1,KT2
         Read(LUNINP,*) RTY(K)
    4   Continue
        GoTo 200
       Endif
*
       If(XTAG.eq.'XY--') Then
        Read(LUNINP,*) KX
        KY = KX
        Do 5  K = 1,KX
         Read(LUNINP,*) RSX(K),RSY(K)
    5   Continue
        GoTo 200
       Endif
*
       If(XTAG.eq.'XYDY') Then
        Read(LUNINP,*) KX,KT1
        KY  = KX
        KDY = KX
        Do 6  K = 1,KX
         Read(LUNINP,*) RSX(K),RSY(K),RSDY(K)
    6   Continue
        GoTo 200
       Endif
*
      GoTo 200
*
*:  errors
*
  997 Continue
      L = MAX0(1,LENX(XPIPE,' '))
      Write(6,1900) XPIPE(1:L),IERR
      GoTo 999
 1900 Format(2X,'+ IO error reading fifo "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check fifo handling')
*
  998 Continue
      L = MAX0(1,LENX(XPIPE,' '))
      Write(6,1901) XPIPE(1:L),IERR
 1901 Format(2X,'+ IO error opening fifo "',A,'"'/
     +       2X,'+ system IO error',I5/
     +       2X,'+ recommended action: check fifo name')
*
*:  print elapsed time
*
  999 CONTINUE
      CALL TIMEC(RT,1,1,0)
      WRITE(6,2011) RT
 2011 FORMAT(
     +  /' -- CPU time :  user ',F6.2,' s  system ',F6.2,' s'/
     +   ' -- PHOENIX module PDSP finished'/)
*
*
      END

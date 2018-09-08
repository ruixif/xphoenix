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
      SUBROUTINE TOPIPE(NW,NERR)
*
*
* purpose :  write data to fifo pipe for graphical display
* date    :  August 14, 2009
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    NW      = determines what to write
*
* export variables :
*    NERR    = error code
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<phox.h>
*
*
      Parameter(PVLIST = 3)
*
      Character*72  XPIPE,XPIPEC
      Character*10  XVLIST(PVLIST)
      Dimension     BVSAPP(PVLIST)
*
      DATA XVLIST(1) / 'PDSP      ' /,
     +     XVLIST(2) / 'XMGRACE   ' /,
     +     NVLIST /2/
*
      Save BPIPE,BVSAPP,XPIPE
*
      NERR = 0
*
      If(NW.ne.1 .and. .not.BPIPE) Return
*
      If(NW.eq.1) Then
*
*:  name of fifo pipe provides vis. app. name
*
       L = LEN(XPIPE)
       Call XFILL(XPIPE,1,L,' ')
       Call GETENV('PHOENIX_PIPE',XPIPE)
*
       XPIPEC = XPIPE
       Call XTOUP(XPIPEC)
       LP1 = Index(XPIPEC,'.ZZ')  + 3
       LP2 = Index(XPIPEC,'PIPE') - 1
*
       BPIPE  = .False.
       If(LP1.gt.0 .and. LP2.gt.0 .and. LP2.ge.LP1) Then
        Do 1  I = 1,NVLIST
         LL = LENX(XVLIST(I),' ')
         If(LL.eq.LP2-LP1+1) Then
          If(XPIPEC(LP1:LP2).eq.XVLIST(I)(1:LL)) Then
           BVSAPP(I) = .True.
          Else
           BVSAPP(I) = .False.
          Endif
         Else
          BVSAPP(I) = .False.
         Endif
         BPIPE = BPIPE .or. BVSAPP(I)
    1   Continue
       Endif
*
       If(.not.BPIPE)  Then
        XMSG = XPIPE
        L1 = LENX(XVLIST(1),' ')
        L2 = LENX(XVLIST(2),' ')
        L3 = LENX(XVLIST(3),' ')
        XMSG1 = XVLIST(1)(1:L1)//
     +           XVLIST(2)(1:L2)//XVLIST(3)(1:L3)
        NERR = 10701
        Return
       Endif
*
*:  open fifo pipe
*
       Open(UNIT=LUNPIP,FILE=XPIPE,STATUS='old',
     +        FORM='formatted',IOSTAT=IERR,ERR=100)
       BPIPE = .True.
       GoTo 101
*
  100  Continue
       BPIPE = .False.
       XMSG  = XPIPE
       NCOM1 = IERR
       NERR  = 10702
       Return
*
  101  Continue
      Endif
*
*:  write data to pipe
*
      If(BVSAPP(1))  Call PUTDSP(NW,NERR)
      If(BVSAPP(2))  Call PUTGRC(NW,NERR)
*
      If(NERR.ne.0) Then
       BPIPE = .False.
       XMSG  = XPIPE
       NERR  = 10703
       Return
      Endif
*
*:  close fifo pipe
*:   -this is needed to flush the output buffer
*
      If(NW.eq.-1 .and. BVSAPP(2))  Then
       Close(LUNPIP)
      Endif
*
*
      End

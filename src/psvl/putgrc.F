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
      Subroutine PUTGRC(NW,NERR)
*
*
* purpose :  write psvl data to fifo pipe for xmgrace
* date    :  August 16, 2009
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    NW      = determines what to write
*    KVEL    = index, data range
*    KTVEL1  = index, beginning of fit range
*    KTVEL2  = index, end of fit range
*    DSX(*)  = energy scale
*    DSY(*)  = Debye sound velocities
*    DSDY(*) = error**2 of Debye sound velocities
*    DTHR(*) = theory function
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<psvl.h>
*
      Character*4   XEXT
      Character*72  XFILE1,XFILE2,XFILE3,XFILE4,XUSER,XH,XF
*
*
      NERR = 0
*
      If(NW.ne.NWW+1) Return
*
      L = Len(XUSER)
      Call XFILL(XUSER,1,L,' ')
      Call GETENV('USER',XUSER)
      L = LENX(XUSER,' ')
      Write(XH,'(''/tmp/'',A,''/.ZZxmgraceTemp. '')') XUSER(1:L)
      LH = LENX(XH,' ')
      XFILE1 = XH(1:LH)//'1'
      XFILE2 = XH(1:LH)//'2'
      XFILE3 = XH(1:LH)//'3'
      XFILE4 = XH(1:LH)//'4'
*
************************************************************
*
*:  write data to temporary files;
*:   find min/max values
*
      DXMIN = DSX(1)
      DXMAX = DSX(KVEL)
      DYMIN =  1.D99
      DYMAX = -1.D99
      Do  K = K1,KVEL
       DYMIN = Dmin1(DYMIN,DSY(K)-Dsqrt(DSDY(K)))
       DYMAX = Dmax1(DYMAX,DSY(K)+Dsqrt(DSDY(K)))
      EndDo
      DYMIN = 1.D-3*DYMIN
      DYMAX = 1.D-3*DYMAX
*
      Open(UNIT=LUNTMP,FILE=XFILE1,FORM='formatted',ERR=999)
*
      Do  K = 1,KVEL
       Write(LUNTMP,'(1P,3E13.5)') DSX(K),1.D-3*DSY(K),
     +                     1.D-3*Dsqrt(DSDY(K))
      EndDo
      Close(UNIT=LUNTMP,ERR=999)
*
      Open(UNIT=LUNTMP,FILE=XFILE2,FORM='formatted',ERR=999)
*
      Do  J = 1,NWW
       Do  K = 1,KIX(J)
        Write(LUNTMP,'(1P,2E13.5)') DSX(K),1.D-3*DTTY(K,J)
       EndDo
       If(J.ne.NWW) Write(LUNTMP,*)
      EndDo
      Close(UNIT=LUNTMP,ERR=999)
*
      DXMIN1 = 0.98*DWW1
      DXMAX1 = 1.02*DWW2
      DYMIN1 =  1.D99
      DYMAX1 = -1.D99
      Do  K = 1,NWW
       DYMIN1 = Dmin1(DYMIN1,DSVT(K)-DDSVT(K))
       DYMAX1 = Dmax1(DYMAX1,DSVT(K)+DDSVT(K))
      EndDo
      DYMIN1 = 1.D-3*DYMIN1
      DYMAX1 = 1.D-3*DYMAX1
*
      DRANGE = DYMAX1 - DYMIN1
      DYMIN1 = DYMIN1 - 0.02*DRANGE
      DYMAX1 = DYMAX1 + 0.02*DRANGE
*
      Open(UNIT=LUNTMP,FILE=XFILE3,FORM='formatted',ERR=999)
*
      Do  K = 1,NWW
       Write(LUNTMP,'(1P,3E13.5)') DWW(K),1.D-3*DSVT(K),
     +                     1.D-3*DDSVT(K)
      EndDo
      Close(UNIT=LUNTMP,ERR=999)
*
      DYMIN2 =  1.D99
      DYMAX2 = -1.D99
      Do  K = 1,NWW
       DYMIN2 = Dmin1(DYMIN2,DCHIT(K))
       DYMAX2 = Dmax1(DYMAX2,DCHIT(K))
      EndDo
*
      DRANGE = DYMAX2 - DYMIN2
      DYMIN2 = DYMIN2 - 0.02*DRANGE
      DYMAX2 = DYMAX2 + 0.02*DRANGE
*
      Open(UNIT=LUNTMP,FILE=XFILE4,FORM='formatted',ERR=999)
*
      Do  K = 1,NWW
       Write(LUNTMP,'(1P,2E13.5)') DWW(K),DCHIT(K)
      EndDo
      Close(UNIT=LUNTMP,ERR=999)
*
**************************************************
*
*:  top graph
*
      If(Index(XOPT,',').eq.0) Then
       XEXT = '.dat'
      Else
       XEXT = '.csv'
      Endif
*
      Write(LUNPIP,'(A)',ERR=998) 'g0 on'
      Write(LUNPIP,'(A)')         'g0 hidden false'
*
      Write(LUNPIP,'(A)') 'with g0'
      Write(LUNPIP,2000)  DXMIN,DYMIN,DXMAX,DYMAX
      Write(LUNPIP,'(A)') 'view 0.1, 0.58, 0.95, 0.94'
      Write(LUNPIP,'(A)') 'title ""'
      Write(LUNPIP,2001)  XOUT(1:LENX(XOUT,' ')),XEXT
      Write(LUNPIP,'(A)') 'subtitle font 4'
      Write(LUNPIP,'(A)') 'subtitle size 1'
      Write(LUNPIP,'(A)') 'legend off'
*
 2000 Format('world',1P,E11.3,',',E11.3,',',E11.3,',',E11.3)
 2001 Format('subtitle "sound velocity fits --> ',A,'_vel(t)',A,'"')
*
*:  data
*
      Do  N = 0,NWW
       Write(XH,*) N
       Call XADJ(XH,' ')
       Write(LUNPIP,'(A,A)') 'kill s',XH(1:LENX(XH,' '))
      EndDo
      Write(LUNPIP,'(A,A,A)') 'read xydy "',XFILE1(1:LH+1),'"'
      Write(LUNPIP,'(A,A,A)') 'read xy "',XFILE2(1:LH+1),'"'
*
*:  axes
*
      Call TICKS(DXMIN,DXMAX,8,DTICK,DTICKM,MTICK,NPREC)
      Write(LUNPIP,'(A)')  'xaxis label "energy (meV)"'
      Write(LUNPIP,'(A)')  'xaxes scale Normal'
      Write(LUNPIP,'(A)')  'xaxis bar linewidth 1.0'
      Write(LUNPIP,'(A)')  'xaxis label font 4'
      Write(LUNPIP,'(A)')  'xaxis label char size 1'
*
      Write(LUNPIP,'(A)')    'xaxis ticklabel font 4'
      Write(LUNPIP,'(A)')    'xaxis ticklabel char size 1'
      Write(LUNPIP,'(A)')    'xaxis ticklabel format decimal'
      Write(LUNPIP,'(A,I3)') 'xaxis ticklabel prec',NPREC
*
      Write(LUNPIP,'(A,1PE13.5)') 'xaxis tick major',DTICK
      Write(LUNPIP,'(A,I3)')      'xaxis tick minor ticks',MTICK
      Write(LUNPIP,'(A)')         'xaxis tick major size 1.0'
      Write(LUNPIP,'(A)')         'xaxis tick minor size 0.5'
      Write(LUNPIP,'(A)')         'xaxis tick place both'
*
      Call TICKS(DYMIN,DYMAX,8,DTICK,DTICKM,MTICK,NPREC)
      Write(LUNPIP,'(A)')  'yaxis label "velocity (km/s)"'
      Write(LUNPIP,'(A)')  'yaxes scale Normal'
      Write(LUNPIP,'(A)')  'yaxis bar linewidth 1.0'
      Write(LUNPIP,'(A)')  'yaxis label font 4'
      Write(LUNPIP,'(A)')  'yaxis label char size 1'
*
      Write(LUNPIP,'(A)')    'yaxis ticklabel font 4'
      Write(LUNPIP,'(A)')    'yaxis ticklabel char size 1'
      Write(LUNPIP,'(A)')    'yaxis ticklabel format decimal'
      Write(LUNPIP,'(A,I3)') 'yaxis ticklabel prec',NPREC
*
      Write(LUNPIP,'(A,1PE13.5)') 'yaxis tick major',DTICK
      Write(LUNPIP,'(A,I3)')      'yaxis tick minor ticks',MTICK
      Write(LUNPIP,'(A)')         'yaxis tick major size 1.0'
      Write(LUNPIP,'(A)')         'yaxis tick minor size 0.5'
      Write(LUNPIP,'(A)')         'yaxis tick place both'
*
*:  set properties
*
      Write(LUNPIP,'(A)') 's0 symbol 1'
      Write(LUNPIP,'(A)') 's0 symbol size 0.4'
      Write(LUNPIP,'(A)') 's0 symbol color 15'
      Write(LUNPIP,'(A)') 's0 symbol pattern 1'
      Write(LUNPIP,'(A)') 's0 symbol fill color 15'
      Write(LUNPIP,'(A)') 's0 symbol fill pattern 8'
      Write(LUNPIP,'(A)') 's0 symbol linewidth 1.0'
      Write(LUNPIP,'(A)') 's0 symbol linestyle 1'
      Write(LUNPIP,'(A)') 's0 symbol skip 0'
      Write(LUNPIP,'(A)') 's0 line type 0'
      Write(LUNPIP,'(A)') 's0 baseline off'
      Write(LUNPIP,'(A)') 's0 dropline off'
      Write(LUNPIP,'(A)') 's0 fill type 0'
      Write(LUNPIP,'(A)') 's0 avalue off'
      Write(LUNPIP,'(A)') 's0 errorbar on'
      Write(LUNPIP,'(A)') 's0 errorbar place both'
      Write(LUNPIP,'(A)') 's0 errorbar color 15'
      Write(LUNPIP,'(A)') 's0 errorbar pattern 1'
      Write(LUNPIP,'(A)') 's0 errorbar size 0.0'
      Write(LUNPIP,'(A)') 's0 errorbar linewidth 1.0'
      Write(LUNPIP,'(A)') 's0 errorbar riser linewidth 1.0'
      Write(LUNPIP,'(A)') 's0 errorbar riser linestyle 1'
*
      Do  J = 1,NWW
       Write(XF,*) J
       Call XADJ(XF,' ')
       L = LENX(XF,' ')
       Write(LUNPIP,'(A,A,A)') 's',XF(1:L),' symbol 0'
       Write(LUNPIP,'(A,A,A)') 's',XF(1:L),' line type 1'
       Write(LUNPIP,'(A,A,A)') 's',XF(1:L),' line linewidth 1.0'
       Write(LUNPIP,'(A,A,A)') 's',XF(1:L),' line pattern 1'
       Write(LUNPIP,'(A,A,A)') 's',XF(1:L),' baseline type 0'
       Write(LUNPIP,'(A,A,A)') 's',XF(1:L),' baseline off'
       Write(LUNPIP,'(A,A,A)') 's',XF(1:L),' dropline off'
       Write(LUNPIP,'(A,A,A)') 's',XF(1:L),' fill type 0'
       Write(LUNPIP,'(A,A,A)') 's',XF(1:L),' avalue off'
       Write(LUNPIP,'(A,A,A)') 's',XF(1:L),' errorbar off'
*
       JJ = 2*(J/15) + 1
       Write(LUNPIP,'(A,A,A,I2)') 's',XF(1:L),' line linestyle',JJ
*
       JJ = Mod(J-1,15) + 1
       Write(LUNPIP,'(A,A,A,I3)') 's',XF(1:L),' line color',JJ
      EndDo
*
**************************************************
*
*:  bottom graphs
*
      Write(LUNPIP,'(A)') 'g1 on'
      Write(LUNPIP,'(A)') 'g1 hidden false'
*
      Write(LUNPIP,'(A)') 'with g1'
      Write(LUNPIP,2000)  DXMIN1,DYMIN1,DXMAX1,DYMAX1
      Write(LUNPIP,'(A)') 'view 0.12, 0.08, 0.9, 0.44'
      Write(LUNPIP,'(A)') 'title ""'
      Write(LUNPIP,2002)  XOUT(1:LENX(XOUT,' ')),XEXT,
     +                    XOUT(1:LENX(XOUT,' ')),XEXT
      Write(LUNPIP,'(A)') 'subtitle font 4'
      Write(LUNPIP,'(A)') 'subtitle size 1'
      Write(LUNPIP,'(A)') 'legend off'
*
 2002 Format('subtitle "fit results --> ',A,'_vld',A,
     +         ' and ',A,'_chi',A,'"')
*
*:  data
*
      Write(LUNPIP,'(A)') 'kill s0'
      Write(LUNPIP,'(A)') 'kill s1'
      Write(LUNPIP,'(A,A,A)') 'read xydy "',XFILE3(1:LH+1),'"'
*
*:  axes
*
      Call TICKS(DXMIN1,DXMAX1,8,DTICK,DTICKM,MTICK,NPREC)
      Write(LUNPIP,'(A)')  'xaxis label "fit window (meV)"'
      Write(LUNPIP,'(A)')  'xaxes scale Normal'
      Write(LUNPIP,'(A)')  'xaxis bar linewidth 1.0'
      Write(LUNPIP,'(A)')  'xaxis label font 4'
      Write(LUNPIP,'(A)')  'xaxis label char size 1'
*
      Write(LUNPIP,'(A)')    'xaxis ticklabel font 4'
      Write(LUNPIP,'(A)')    'xaxis ticklabel char size 1'
      Write(LUNPIP,'(A)')    'xaxis ticklabel format decimal'
      Write(LUNPIP,'(A,I3)') 'xaxis ticklabel prec',NPREC
*
      Write(LUNPIP,'(A,1PE13.5)') 'xaxis tick major',DTICK
      Write(LUNPIP,'(A,I3)')      'xaxis tick minor ticks',MTICK
      Write(LUNPIP,'(A)')         'xaxis tick major size 1.0'
      Write(LUNPIP,'(A)')         'xaxis tick minor size 0.5'
      Write(LUNPIP,'(A)')         'xaxis tick place both'
*
      Call TICKS(DYMIN1,DYMAX1,8,DTICK,DTICKM,MTICK,NPREC)
      Write(LUNPIP,'(A)')  'yaxis label "velocity (km/s)"'
      Write(LUNPIP,'(A)')  'yaxes scale Normal'
      Write(LUNPIP,'(A)')  'yaxis bar linewidth 1.0'
      Write(LUNPIP,'(A)')  'yaxis label font 4'
      Write(LUNPIP,'(A)')  'yaxis label char size 1'
*
      Write(LUNPIP,'(A)')    'yaxis ticklabel font 4'
      Write(LUNPIP,'(A)')    'yaxis ticklabel char size 1'
      Write(LUNPIP,'(A)')    'yaxis ticklabel format decimal'
      Write(LUNPIP,'(A,I3)') 'yaxis ticklabel prec',NPREC
*
      Write(LUNPIP,'(A,1PE13.5)') 'yaxis tick major',DTICK
      Write(LUNPIP,'(A,I3)')      'yaxis tick minor ticks',MTICK
      Write(LUNPIP,'(A)')         'yaxis tick major size 1.0'
      Write(LUNPIP,'(A)')         'yaxis tick minor size 0.5'
      Write(LUNPIP,'(A)')         'yaxis tick place normal'
*
*:  set properties
*
      Write(LUNPIP,'(A)') 's0 symbol 1'
      Write(LUNPIP,'(A)') 's0 symbol size 0.6'
      Write(LUNPIP,'(A)') 's0 symbol color 4'
      Write(LUNPIP,'(A)') 's0 symbol pattern 1'
      Write(LUNPIP,'(A)') 's0 symbol fill color 15'
      Write(LUNPIP,'(A)') 's0 symbol fill pattern 8'
      Write(LUNPIP,'(A)') 's0 symbol linewidth 1.0'
      Write(LUNPIP,'(A)') 's0 symbol linestyle 1'
      Write(LUNPIP,'(A)') 's0 symbol skip 0'
      Write(LUNPIP,'(A)') 's0 line type 0'
      Write(LUNPIP,'(A)') 's0 baseline off'
      Write(LUNPIP,'(A)') 's0 dropline off'
      Write(LUNPIP,'(A)') 's0 fill type 0'
      Write(LUNPIP,'(A)') 's0 avalue off'
      Write(LUNPIP,'(A)') 's0 errorbar on'
      Write(LUNPIP,'(A)') 's0 errorbar place both'
      Write(LUNPIP,'(A)') 's0 errorbar color 4'
      Write(LUNPIP,'(A)') 's0 errorbar pattern 1'
      Write(LUNPIP,'(A)') 's0 errorbar size 0.0'
      Write(LUNPIP,'(A)') 's0 errorbar linewidth 1.0'
      Write(LUNPIP,'(A)') 's0 errorbar riser linewidth 1.0'
      Write(LUNPIP,'(A)') 's0 errorbar riser linestyle 1'
*
*
      Write(LUNPIP,'(A)') 'g2 on'
      Write(LUNPIP,'(A)') 'g2 hidden false'
*
      Write(LUNPIP,'(A)') 'with g2'
      Write(LUNPIP,2000)  DXMIN1,DYMIN2,DXMAX1,DYMAX2
      Write(LUNPIP,'(A)') 'view 0.12, 0.08, 0.9, 0.44'
      Write(LUNPIP,'(A)') 'title ""'
      Write(LUNPIP,'(A)') 'subtitle ""'
*
*:  data
*
      Write(LUNPIP,'(A)') 'kill s0'
      Write(LUNPIP,'(A,A,A)') 'read xy "',XFILE4(1:LH+1),'"'
*
*:  axes
*
      Write(LUNPIP,'(A)')  'xaxis label ""'
      Write(LUNPIP,'(A)')  'xaxes scale Normal'
      Write(LUNPIP,'(A)')  'xaxis bar linewidth 1.0'
      Write(LUNPIP,'(A)')  'xaxis tick off'
      Write(LUNPIP,'(A)')  'xaxis ticklabel off'
*
      Call TICKS(DYMIN2,DYMAX2,8,DTICK,DTICKM,MTICK,NPREC)
      Write(LUNPIP,'(A)')  'yaxis label "\xc\4\S2\N"'
      Write(LUNPIP,'(A)')  'yaxes scale Normal'
      Write(LUNPIP,'(A)')  'yaxis bar linewidth 1.0'
      Write(LUNPIP,'(A)')  'yaxis label font 4'
      Write(LUNPIP,'(A)')  'yaxis label char size 1'
      Write(LUNPIP,'(A)')  'yaxis label place opposite'
*
      Write(LUNPIP,'(A)')    'yaxis ticklabel font 4'
      Write(LUNPIP,'(A)')    'yaxis ticklabel char size 1'
      Write(LUNPIP,'(A)')    'yaxis ticklabel format decimal'
      Write(LUNPIP,'(A,I3)') 'yaxis ticklabel prec',NPREC
      Write(LUNPIP,'(A)')    'yaxis ticklabel place opposite'
*
      Write(LUNPIP,'(A,1PE13.5)') 'yaxis tick major',DTICK
      Write(LUNPIP,'(A,I3)')      'yaxis tick minor ticks',MTICK
      Write(LUNPIP,'(A)')         'yaxis tick major size 1.0'
      Write(LUNPIP,'(A)')         'yaxis tick minor size 0.5'
      Write(LUNPIP,'(A)')         'yaxis tick place opposite'
*
*:  set properties
*
      Write(LUNPIP,'(A)') 's0 symbol 2'
      Write(LUNPIP,'(A)') 's0 symbol size 0.6'
      Write(LUNPIP,'(A)') 's0 symbol color 2'
      Write(LUNPIP,'(A)') 's0 symbol pattern 1'
      Write(LUNPIP,'(A)') 's0 symbol fill color 2'
      Write(LUNPIP,'(A)') 's0 symbol fill pattern 8'
      Write(LUNPIP,'(A)') 's0 symbol linewidth 1.0'
      Write(LUNPIP,'(A)') 's0 symbol linestyle 1'
      Write(LUNPIP,'(A)') 's0 symbol skip 0'
      Write(LUNPIP,'(A)') 's0 line type 0'
      Write(LUNPIP,'(A)') 's0 baseline off'
      Write(LUNPIP,'(A)') 's0 dropline off'
      Write(LUNPIP,'(A)') 's0 fill type 0'
      Write(LUNPIP,'(A)') 's0 avalue off'
      Write(LUNPIP,'(A)') 's0 errorbar off'
*
      Write(LUNPIP,'(A)') 'updateall'
      Write(LUNPIP,'(A)') 'redraw'
      Return
*
  998 Continue
      NERR = 1
*
  999 Continue
*
*
      END

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
* purpose :  write data to fifo pipe for xmgrace
* date    :  August 16, 2009
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    NW      = determines what to write
*    KD      = index, data range
*    KPD1    = index for E=0
*    KPD2    = index, end of DOS range
*    DSX(*)  = energy scale
*    DSY(*)  = measured data, peak subtracted
*    DSDY(*) = error**2 of measured data
*    DCY(*)  = DOS
*    DCDY(*) = error of DOS
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<phox.h>
*
*
      Character*72  XFILE1,XFILE2,XFILE3,XUSER,XH
*
      NERR = 0
*
      If(NW.ne.2) Return
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
*
************************************************************
*
*:  write data to temporary files;
*:   find min/max values
*
      Open(UNIT=LUNTMP,FILE=XFILE1,FORM='formatted',ERR=999)
*
      DXMIN = -(DSX(KD)-DSX(1))/6.D0
      DXMAX =  (DSX(KD)-DSX(1))/6.D0
      DYMIN =  1.D99
      DYMAX = -1.D99
      Do  K = 1,KD
       If(DSX(K).ge.DXMIN .and. DSX(K).le.DXMAX) Then
        DYMIN = Dmin1(DYMIN,DSY(K)-Dsqrt(DSDY(K)))
        DYMAX = Dmax1(DYMAX,DSY(K)+Dsqrt(DSDY(K)))
        Write(LUNTMP,'(1P,3E13.5)') DSX(K),
     +              DSY(K)/DSCLE,Dsqrt(DSDY(K))/DSCLE
       Endif
      EndDo
      DYMIN = DYMIN/DSCLE
      DYMAX = DYMAX/DSCLE
      Close(UNIT=LUNTMP,ERR=999)
*
      Open(UNIT=LUNTMP,FILE=XFILE2,FORM='formatted',ERR=999)
*
      DXMIN1 = DSX(KPD1)
      DXMAX1 = DSX(KD)
      DYMIN1 =  1.D99
      DYMAX1 = -1.D99
      Do  K = 1,KD-KPD1+1
       DYMIN1 = Dmin1(DYMIN1,DCY(K)-DCDY(K))
       DYMAX1 = Dmax1(DYMAX1,DCY(K)+2.D0*DCDY(K))
       Write(LUNTMP,'(1P,3E13.5)') DSX(K+KPD1-1),DCY(K),DCDY(K)
      EndDo
      Close(UNIT=LUNTMP,ERR=999)
*
      Open(UNIT=LUNTMP,FILE=XFILE3,FORM='formatted',ERR=999)
*
      DFAC   = 0.85D0*DYMAX1*(DSX(2)-DSX(1))/3.D3
      DOSUM  = 0.D0
      DDOSUM = 0.D0
      Do  K = 1,KD-KPD1+1
       Write(LUNTMP,'(1P,3E13.5)') DSX(K+KPD1-1),
     +               DOSUM*DFAC,Dsqrt(DDOSUM)*DFAC
       DOSUM  = DOSUM + DCY(K)
       DDOSUM = DDOSUM + DCDY(K)**2
      EndDo
      Close(UNIT=LUNTMP,ERR=999)
*
**************************************************
*
*:  top graph
*
      XH = XOUT
      If(Index(XOPT,',').eq.0) Then
       Call XMERGE(XH,'_pst.dat')
      Else
       Call XMERGE(XH,'_pst.csv')
      Endif
      L = LENX(XH,' ')
*
      Write(LUNPIP,'(A)',ERR=998) 'g0 on'
      Write(LUNPIP,'(A)')         'g0 hidden false'
*
      Write(LUNPIP,'(A)') 'with g0'
      Write(LUNPIP,2000)  DXMIN,DYMIN,DXMAX,DYMAX
      Write(LUNPIP,'(A)') 'view 0.1, 0.62, 0.95, 0.94'
      Write(LUNPIP,'(A)') 'title ""'
      Write(LUNPIP,2001)  XH(1:L)
      Write(LUNPIP,'(A)') 'subtitle font 4'
      Write(LUNPIP,'(A)') 'subtitle size 1'
      Write(LUNPIP,'(A)') 'legend off'
*
 2000 Format('world',1P,E11.3,',',E11.3,',',E11.3,',',E11.3)
 2001 Format('subtitle "peak subtraction --> ',A,'"')
*
*:  data
*
      Do  N = 0,20
       Write(XH,*) N
       Call XADJ(XH,' ')
       Write(LUNPIP,'(A,A)') 'kill s',XH(1:LENX(XH,' '))
      EndDo
      Write(LUNPIP,'(A,A,A)') 'read xydy "',XFILE1(1:LH+1),'"'
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
      Write(LUNPIP,'(A)')  'yaxis label "counts"'
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
**************************************************
*
*:  bottom graph
*
      XH = XOUT
      If(Index(XOPT,',').eq.0) Then
       Call XMERGE(XH,'_dos.dat')
      Else
       Call XMERGE(XH,'_dos.csv')
      Endif
      L = LENX(XH,' ')
*
      Write(LUNPIP,'(A)') 'g1 on'
      Write(LUNPIP,'(A)') 'g1 hidden false'
      Write(LUNPIP,'(A)') 'with g1'
      Write(LUNPIP,'(A)') 'view 0.1, 0.08, 0.95, 0.48'
      Write(LUNPIP,2000)  DXMIN1,DYMIN1,DXMAX1,DYMAX1
      Write(LUNPIP,'(A)') 'title ""'
      Write(LUNPIP,2002)  XH(1:L)
      Write(LUNPIP,'(A)') 'subtitle font 4'
      Write(LUNPIP,'(A)') 'subtitle size 1'
      Write(LUNPIP,'(A)') 'legend off'
*
 2002 Format('subtitle "partial phonon DOS --> ',A,'"')
*
*:  data
*
      Write(LUNPIP,'(A)')     'kill s0'
      Write(LUNPIP,'(A)')     'kill s1'      
      Write(LUNPIP,'(A,A,A)') 'read xydy "',XFILE2(1:LH+1),'"'
      Write(LUNPIP,'(A,A,A)') 'read xydy "',XFILE3(1:LH+1),'"'
*
*:  axes
*
      Call TICKS(DXMIN1,DXMAX1,8,DTICK,DTICKM,MTICK,NPREC)
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
      Call TICKS(DYMIN1,DYMAX1,8,DTICK,DTICKM,MTICK,NPREC)
      Write(LUNPIP,'(A)')  'yaxis label "PDOS (1/eV)"'
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
      Write(LUNPIP,'(A)') 's0 symbol color 4'
      Write(LUNPIP,'(A)') 's0 symbol pattern 1'
      Write(LUNPIP,'(A)') 's0 symbol fill color 4'
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
      Write(LUNPIP,'(A)') 's1 symbol 0'
      Write(LUNPIP,'(A)') 's1 line type 1'
      Write(LUNPIP,'(A)') 's1 line linestyle 1'
      Write(LUNPIP,'(A)') 's1 line linewidth 2.0'
      Write(LUNPIP,'(A)') 's1 line color 2'
      Write(LUNPIP,'(A)') 's1 line pattern 1'
      Write(LUNPIP,'(A)') 's1 baseline off'
      Write(LUNPIP,'(A)') 's1 dropline off'
      Write(LUNPIP,'(A)') 's1 fill type 0'
      Write(LUNPIP,'(A)') 's1 avalue off'
      Write(LUNPIP,'(A)') 's1 errorbar on'
      Write(LUNPIP,'(A)') 's1 errorbar place both'
      Write(LUNPIP,'(A)') 's1 errorbar color 2'
      Write(LUNPIP,'(A)') 's1 errorbar pattern 1'
      Write(LUNPIP,'(A)') 's1 errorbar size 0.0'
      Write(LUNPIP,'(A)') 's1 errorbar linewidth 1.0'
      Write(LUNPIP,'(A)') 's1 errorbar riser linewidth 1.0'
      Write(LUNPIP,'(A)') 's1 errorbar riser linestyle 1'
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
      End

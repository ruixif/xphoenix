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
*    NW       = determines what to write
*    NENG     = index, energy range
*    DSX(*)   = energy scale
*    DSY(*,0) = phonon excitation probability
*    DSY(*,N) = N-phonon excitation probability
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<psth.h>
*
      Character*4   XEXT
      Character*72  XFILE1,XFILE2,XUSER,XH,XF
*
*
      NERR = 0
*
      If(NW.ne.JMAX+1) Return
*
      L = Len(XUSER)
      Call XFILL(XUSER,1,L,' ')
      Call GETENV('USER',XUSER)
      L = LENX(XUSER,' ')
      Write(XH,'(''/tmp/'',A,''/.ZZxmgraceTemp. '')') XUSER(1:L)
      LH = LENX(XH,' ')
      XFILE1 = XH(1:LH)//'1'
      XFILE2 = XH(1:LH)//'2'
*
************************************************************
*
*:  write data to temporary files;
*:   find min/max values
*
      DXMIN = DSX(1)
      DXMAX = DSX(NENG)
      DYMIN = 0.D0
      DYMAX = -1.D99
      Do  K = 1,NENG
       DYMAX = Dmax1(DYMAX,DSY(K,0))
      EndDo
*
      Open(UNIT=LUNTMP,FILE=XFILE1,FORM='formatted',ERR=999)
*
      Do  K = 1,NENG
       Write(LUNTMP,'(1P,2E13.5)') DSX(K),DSY(K,0)
      EndDo
      Close(UNIT=LUNTMP,ERR=999)
*
      DYMIN1 =  1.D99
      DYMAX1 = -1.D99
      Do  J = 1,JMAX
       Do  K = 1,NENG
        DYMIN1 = Dmin1(DYMIN1,DSY(K,J))
        DYMAX1 = Dmax1(DYMAX1,DSY(K,J))
       EndDo
      EndDo
      DYMIN1 = Dmax1(DYMIN1,5.D-4*DYMAX1)
*
      Open(UNIT=LUNTMP,FILE=XFILE2,FORM='formatted',ERR=999)
*
      Do  J = 1,JMAX
       Do  K = 1,NENG
        Write(LUNTMP,'(1P,2E13.5)') DSX(K),DSY(K,J)
       EndDo
       If(J.ne.JMAX) Write(LUNTMP,*)
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
      Write(LUNPIP,'(A)',ERR=998) 'g1 on'
      Write(LUNPIP,'(A)')         'g1 hidden false'
*
      Write(LUNPIP,'(A)') 'with g1'
      Write(LUNPIP,2000)  DXMIN,DYMIN,DXMAX,DYMAX
      Write(LUNPIP,'(A)') 'view 0.1, 0.58, 0.95, 0.94'
      Write(LUNPIP,'(A)') 'title ""'
      Write(LUNPIP,2001)  XOUT(1:LENX(XOUT,' ')),XEXT
      Write(LUNPIP,'(A)') 'subtitle font 4'
      Write(LUNPIP,'(A)') 'subtitle size 1'
      Write(LUNPIP,'(A)') 'legend off'
*
 2000 Format('world',1P,E11.3,',',E11.3,',',E11.3,',',E11.3)
 2001 Format('subtitle "phonon excitation probability --> ',A
     +           ,'_psn',A,'"')
*
*:  data
*
      Write(LUNPIP,'(A)') 'kill s0'
      Write(LUNPIP,'(A)') 'kill s1'
      Write(LUNPIP,'(A,A,A)') 'read xy "',XFILE1(1:LH+1),'"'
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
      Write(LUNPIP,'(A)')  'yaxis label "S(E) (1/eV)"'
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
      Write(LUNPIP,'(A)') 's0 symbol 0'
      Write(LUNPIP,'(A)') 's0 line type 1'
      Write(LUNPIP,'(A)') 's0 line linestyle 1'
      Write(LUNPIP,'(A)') 's0 line linewidth 1.0'
      Write(LUNPIP,'(A)') 's0 line color 15'
      Write(LUNPIP,'(A)') 's0 line pattern 1'
      Write(LUNPIP,'(A)') 's0 baseline off'
      Write(LUNPIP,'(A)') 's0 dropline off'
      Write(LUNPIP,'(A)') 's0 fill type 0'
      Write(LUNPIP,'(A)') 's0 avalue off'
      Write(LUNPIP,'(A)') 's0 errorbar off'
*
**************************************************
*
*:  bottom graph
*
      Write(LUNPIP,'(A)',ERR=998) 'g0 on'
      Write(LUNPIP,'(A)')         'g0 hidden false'
*
      Write(LUNPIP,'(A)') 'with g0'
      Write(LUNPIP,2000)  DXMIN,DYMIN1,DXMAX,DYMAX1
      Write(LUNPIP,'(A)') 'view 0.1, 0.08, 0.95, 0.44'
      Write(LUNPIP,'(A)') 'title ""'
      Write(LUNPIP,2002)  XOUT(1:LENX(XOUT,' ')),XEXT
      Write(LUNPIP,'(A)') 'subtitle font 4'
      Write(LUNPIP,'(A)') 'subtitle size 1'
      Write(LUNPIP,'(A)') 'legend off'
*
 2002 Format('subtitle "n-phonon contributions --> ',A,'_mpc',A,'"')
*
*:  data
*
      Do  N = 1,JMAX
       Write(XH,*) N-1
       Call XADJ(XH,' ')
       Write(LUNPIP,'(A,A)') 'kill s',XH(1:LENX(XH,' '))
      EndDo
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
      Write(LUNPIP,'(A)')  'yaxis label "S\sn\N(E) (1/eV)"'
      Write(LUNPIP,'(A)')  'yaxes scale Logarithmic'
      Write(LUNPIP,'(A)')  'yaxis tick major 10'
      Write(LUNPIP,'(A)')  'yaxis tick minor ticks 9'
      Write(LUNPIP,'(A)')  'yaxis ticklabel format power'
      Write(LUNPIP,'(A)')  'yaxis ticklabel prec 0'
      Write(LUNPIP,'(A)')  'yaxis bar linewidth 1.0'
      Write(LUNPIP,'(A)')  'yaxis label font 4'
      Write(LUNPIP,'(A)')  'yaxis label char size 1'
      Write(LUNPIP,'(A)')  'yaxis ticklabel font 4'
      Write(LUNPIP,'(A)')  'yaxis ticklabel char size 1'
      Write(LUNPIP,'(A)')  'yaxis tick major size 0.7'
*
*:  set properties
*
      Do  J = 1,JMAX
       Write(XF,*) J-1
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
*
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

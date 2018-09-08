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
* purpose :  write padd data to fifo pipe for xmgrace
* date    :  August 16, 2009
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    NW      = determines what to write
*    KDS     = index, data range
*    DSX(*)  = energy scale
*    DSY(*)  = counts
*    DSDY(*) = error**2 of counts
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
*
*
      Character*72  XFILE,XUSER,XH
*
      NERR = 0
*
      If(NW.ne.1) Return
*
      L = Len(XUSER)
      Call XFILL(XUSER,1,L,' ')
      Call GETENV('USER',XUSER)
      L = LENX(XUSER,' ')
      Write(XFILE,'(''/tmp/'',A,''/.ZZxmgraceTemp '')') XUSER(1:L)
      LH = LENX(XFILE,' ')
*
************************************************************
*
*:  write data to temporary files;
*:   find min/max values
*
      Open(UNIT=LUNTMP,FILE=XFILE,FORM='formatted',IOSTAT=IERR,ERR=999)
*
      Do  K = 1,KDS
       Write(LUNTMP,'(1P,3E13.5)') DSX(K),DSY(K),Dsqrt(DSDY(K))
      EndDo
      Close(UNIT=LUNTMP,ERR=999)
*
      DXMIN = DSX(1)
      DXMAX = DSX(KDS)
      DYMIN =  1.D99
      DYMAX = -1.D99
      Do  K = 1,KDS
       DYMIN = Dmin1(DYMIN,DSY(K)-0.5D0*Dsqrt(DSDY(K)))
       If(Dabs(DSX(K)).gt.DRANGE) Then
        DYMAX = Dmax1(DYMAX,DSY(K)+0.5D0*Dsqrt(DSDY(K)))
       Endif
      EndDo
      DYMAX = 1.05D0*DYMAX
*
      XH = XOUT
      If(Index(XOPT,',').eq.0) Then
       Call XMERGE(XH,'_sum.dat')
      Else
       Call XMERGE(XH,'_sum.csv')
      Endif
      L = LENX(XH,' ')
*
      Write(LUNPIP,'(A)',ERR=998) 'kill g1'
      Write(LUNPIP,'(A)')         'g0 on'
      Write(LUNPIP,'(A)')         'g0 hidden false'
      Write(LUNPIP,'(A)')         'with g0'
      Write(LUNPIP,'(A)') 'view 0.1, 0.08, 0.95, 0.93'
      Write(LUNPIP,2000)  DXMIN,DYMIN,DXMAX,DYMAX
      Write(LUNPIP,'(A)') 'title ""'
      Write(LUNPIP,2001)  XH(1:L)
      Write(LUNPIP,'(A)') 'subtitle font 4'
      Write(LUNPIP,'(A)') 'subtitle size 1'
      Write(LUNPIP,'(A)') 'legend off'
*
 2000 Format('world',1P,E11.3,',',E11.3,',',E11.3,',',E11.3)
 2001 Format('subtitle "added data --> ',A,'"')
*
*:  data
*
      Do  N = 0,20
       Write(XH,*) N
       Call XADJ(XH,' ')
       Write(LUNPIP,'(A,A)') 'kill s',XH(1:LENX(XH,' '))
      EndDo
      Write(LUNPIP,'(A,A,A)') 'read xydy "',XFILE(1:LH),'"'
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
      Write(LUNPIP,'(A)') 's0 symbol size 0.3'
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

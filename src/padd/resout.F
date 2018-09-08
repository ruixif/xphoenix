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
      Subroutine RESOUT(XFILE,ICC1,ICC2,ICM,BNORM)
*
* purpose :  print some results
* date    :  October 29, 2016
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    XOUT  = output file prefix
*    XOPT  = output options
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
#include<xterm.h>
*
      Character*(*) XFILE
      Character*4   XEXT
      Character*72  XF
*
*
*:  print zero-energy position
*
      If(BPEAK) Then
       L  = LENX(XFILE,' ')
       L1 = Max0(1,L-20)
       If(L1.gt.1) Then
        Write(6,2000) XT_MAG,'...'//XFILE(L1:L),XT_DEF
        Write(LUNPTL,2000) '','...'//XFILE(L1:L),''
       Else
        Write(6,2000) XT_MAG,XFILE(L1:L),XT_DEF
        Write(LUNPTL,2000) '',XFILE(L1:L),''
       Endif
       If(DA1E0.NE.1.D66) Then
        Write(6,2001) DA1E0
        Write(LUNPTL,2001) DA1E0
       Endif
       If(ICC1.ne.0) Then
        Write(6,2002) DC1E0/DCSCF
        Write(LUNPTL,2002) DC1E0/DCSCF
       Endif
       If(DT1E0.NE.1.D66) Then
        Write(6,2003) DT1E0
        Write(LUNPTL,2003) DT1E0
       Endif
       If(DA2E0.NE.1.D66) Then
        Write(6,2004) DA2E0
        Write(LUNPTL,2004) DA2E0
       Endif
       If(ICC2.ne.0) Then
        Write(6,2005) DC2E0/DCSCF
        Write(LUNPTL,2005) DC2E0/DCSCF
       Endif
       If(DT2E0.NE.1.D66) Then
        Write(6,2006) DT2E0
        Write(LUNPTL,2006) DT2E0
       Endif
       If(DT3E0.NE.1.D66) Then
        Write(6,2007) DT3E0
        Write(LUNPTL,2007) DT3E0
       Endif
      Endif
*
 2000 Format(2X,A,'Retraced peak position from file "',A,'" -->',A)
 2001 Format(4X,'1.crystal goniometer position',F15.3)
 2002 Format(4X,'1.crystal encoder reading    ',F15.3)
 2003 Format(4X,'1.crystal temperature        ',F15.3)
 2004 Format(4X,'2.crystal goniometer position',F15.3)
 2005 Format(4X,'2.crystal encoder reading    ',F15.3)
 2006 Format(4X,'2.crystal temperature        ',F15.3)
 2007 Format(4X,'analyzer temperature         ',F15.3)
*
*:  print detailed imbalance, energy range, inelastic counts
*
      If(BNORM) Then
       Write(6,2008) XT_MAG,XT_DEF
       Write(LUNPTL,2008) '',''
      Else
       Write(6,2009) XT_MAG,XT_DEF
       Write(LUNPTL,2009) '',''
      Endif
*
 2008 Format(/2X,A,'Results of spectra addition -->',A/
     +   4X,'%imbalance',6X,'range (meV)',5X,'counts',
     +     4X,'%drift',3X,'file')
 2009 Format(/2X,A,'Results of spectra addition -->',A/
     +   4X,'%imbalance',6X,'range (meV)',5X,'counts',7X,'file')
*
      Do  I = 1,NCL
       If(.not.BDS(I)) Cycle
*
       If(NICS(I).ne.0) Then
        L = LENX(XMES(I),' ')
        Write(XF,'(A,''  #'',I4.4,1X)') XMES(I)(1:L),NICS(I)
       Else
        XF = XMES(I)
       Endif
       L = LENX(XF,' ')
       If(L.ge.44) Then
        XF(1:44) = '...'//XF(L-40:L)
        L = 44
       Endif
*
       ITOTAL = Idnint(DMONAV(I)*(DTOTAL(I) - DTOTAL(0)))
       IMSIDE = Idnint(DMONAV(I)*(DMSIDE(I) - DMSIDE(0)))
       If(BNORM) Then
        If(DFLC(I).ge.10.D0) Then
         Write(6,2010) 1.D2*DTBH(I),1.D2*DDTBH(I),
     +       DEVON(I),DEBIS(I),ITOTAL,IMSIDE,
     +       XT_RED,DFLC(I),XT_DEF,XF(1:L)
        Else
         Write(6,2010) 1.D2*DTBH(I),1.D2*DDTBH(I),
     +       DEVON(I),DEBIS(I),ITOTAL,IMSIDE,
     +       '',DFLC(I),'',XF(1:L)
        Endif
        Write(LUNPTL,2010) 1.D2*DTBH(I),1.D2*DDTBH(I),
     +       DEVON(I),DEBIS(I),ITOTAL,IMSIDE,
     +       '',DFLC(I),'',XF(1:L)
       Else
        Write(6,2011) 1.D2*DTBH(I),1.D2*DDTBH(I),
     +       DEVON(I),DEBIS(I),ITOTAL,IMSIDE,
     +       XF(1:L)
        Write(LUNPTL,2011) 1.D2*DTBH(I),1.D2*DDTBH(I),
     +       DEVON(I),DEBIS(I),ITOTAL,IMSIDE,
     +       XF(1:L)
       Endif
      EndDo
*
 2010 Format(F8.1,' +-',F4.1,2X,2F7.1,I8,I5,A,F7.2,A,3X,A)
 2011 Format(F8.1,' +-',F4.1,2X,2F7.1,I8,I5,3X,A)
*
*
      DTBSUM = 0.D0
      DDTBSM = 0.D0
      DNTBSM = 0.D0
      DIFLSM = 0.D0
      DMONSM = 0.D0
      II     = 0
      Do  I = 1,NCL
       If(.not.BDS(I)) Cycle
       II     = II + 1
       DTBSUM = DTBSUM + DTBH(I)*DNTBH(I)
       DDTBSM = DDTBSM + (DDTBH(I)*DNTBH(I))**2
       DNTBSM = DNTBSM + DNTBH(I)
       DIFLSM = DIFLSM + DFLC(I)**2
       DMONSM = DMONSM + DMONAV(I)
      EndDo
      DTBSUM = DTBSUM/DNTBSM
      DDTBSM = Dsqrt(DDTBSM)/DNTBSM
      DIFLSM = Dsqrt(DIFLSM/Dfloat(II))
*
      DTOSUM =  0.D0
      DMSSUM = -1.D10
      Do  K = 1,KDS
       If(DSX(K).lt.-DRANGE .or. DSX(K).gt.DRANGE) Then
         DTOSUM = DTOSUM + DSY(K)
         DMSSUM = Dmax1(DMSSUM,DSY(K))
       Endif
      EndDo
      DTOSUM = DTOSUM - DMONSM*DTOTAL(0)
      DMSSUM = DMSSUM - DMONSM*DMSIDE(0)
*
      XF = XOUT
      If(Index(XOPT,',').eq.0) Then
       Call XMERGE(XF,'_sum.dat')
      Else
       Call XMERGE(XF,'_sum.csv')
      Endif
      L = LENX(XF,' ')
      If(L.ge.44) Then
       XF(1:44) = '...'//XF(L-40:L)
       L = 44
      Endif   
*
      If(BNORM) Then
       If(DIFLSM.ge.10.D0) Then
        Write(6,2010) 1.D2*DTBSUM,1.D2*DDTBSM,
     +       DSX(1),DSX(KDS),Idnint(DTOSUM),
     +       Idnint(DMSSUM),XT_RED,DIFLSM,XT_DEF,XF(1:L)
       Else
        Write(6,2010) 1.D2*DTBSUM,1.D2*DDTBSM,
     +       DSX(1),DSX(KDS),Idnint(DTOSUM),
     +       Idnint(DMSSUM),'',DIFLSM,'',XF(1:L)
       Endif
       Write(LUNPTL,2010) 1.D2*DTBSUM,1.D2*DDTBSM,
     +       DSX(1),DSX(KDS),Idnint(DTOSUM),
     +       Idnint(DMSSUM),'',DIFLSM,'',XF(1:L)
      Else
       Write(6,2011) 1.D2*DTBSUM,1.D2*DDTBSM,
     +       DSX(1),DSX(KDS),Idnint(DTOSUM),Idnint(DMSSUM),
     +       XF(1:L)
       Write(LUNPTL,2011) 1.D2*DTBSUM,1.D2*DDTBSM,
     +       DSX(1),DSX(KDS),Idnint(DTOSUM),Idnint(DMSSUM),
     +       XF(1:L)
      Endif
*
*:  list of created files
*
      If(Index(XOPT,',').eq.0) Then
       XEXT = '.dat'
      Else
       XEXT = '.csv'
      Endif
      L = LENX(XOUT,' ')
      Write(LUNPTL,2012) XOUT(1:L)//'_padd_ptl.txt'
 2012 Format(//2X,'Created files -->'//
     +       3X,'Protocol of calculations      : ',A)
*
      L = L + 8
      If(BNORM) Then
       XFILE = XOUT
       If(ICM.eq.0) Then
        Call XMERGE(XFILE,'_nrm'//XEXT)
        Write(LUNPTL,2013) XFILE(1:L)
       Else
        Call XMERGE(XFILE,'_mon'//XEXT)
        Write(LUNPTL,2014) XFILE(1:L)
       Endif
      Endif
 2013 Format(3X,'Normalization functions       : ',A)
 2014 Format(3X,'Monitor functions             : ',A)
*
      XFILE = XOUT
      Call XMERGE(XFILE,'_shf'//XEXT)
      Write(LUNPTL,2015) XFILE(1:L)
 2015 Format(3X,'Spectra with peaks at E=0     : ',A)
*
      XFILE = XOUT
      Call XMERGE(XFILE,'_sum'//XEXT)
      Write(LUNPTL,2016) XFILE(1:L)
 2016 Format(3X,'Sum of spectra w/ peak at E=0 : ',A)
*
*
      End

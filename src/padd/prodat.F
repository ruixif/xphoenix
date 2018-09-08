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
      Subroutine PRODAT(IDS,ICE,ICD,ICA1,ICA2,ICC1,ICC2,
     +                   ICT1,ICT2,ICT3,ICN,ICM,ICP,ICS)
*
* Purpose :  Process data, remove noise spikes
*            The meaning of the columns was defined for each
*            input file.
* Date    :  October 28, 2016
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    IDS        = file number
*    ICE        = col. for energy
*    ICD        = col. for counts
*    ICA1       = col. for angle position of first crystal
*    ICA2       = col. for angle position of second crystal
*    ICC1       = col. for angle calibration of first crystal
*    ICC2       = col. for angle calibration of second crystal
*    ICT1       = col. for temperature of first crystal
*    ICT2       = col. for temperature of second crystal
*    ICT3       = col. for temperature of analyzer crystal
*    ICN        = col. to normalize counts
*    ICM        = col. for count monitor
*    ICP        = col. for elastic peak spectrum
*    ICS        = scan number
*    XMES(IDS)  = name of input file no. <IDS>
*    KD         = nr. of data points
*    DBGRD      = background
*    DMZ(*)     = counts
*    DMY(*)     = counts of elastic peak spectrum
*
* export variables :
*    DMZ(*)     = counts, spikes removed
*    DMY(*)     = counts of elastic peak spectrum, spikes removed
*    DMDZ(*)    = errors^2 of counts, spikes removed
*    DMDY(*)    = errors^2 of elastic peak spectrum, spikes removed
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
#include<xterm.h>
*
*
*:  reset errors
*
      NERR = 0
*
*:  printout
*
      L = LENX(XMES(IDS),' ')
      If(ICS.eq.0) Then
       Write(6,2000) XT_MAG,XMES(IDS)(1:L),XT_DEF,KD,DBGRD,ICD
       Write(LUNPTL,2000) '',XMES(IDS)(1:L),'',KD,DBGRD,ICD
      Else
       Write(6,2001) XT_MAG,XMES(IDS)(1:L),ICS,XT_DEF,KD,DBGRD,ICD
       Write(LUNPTL,2001) '',XMES(IDS)(1:L),ICS,'',KD,DBGRD,ICD
      Endif
*
 2000 Format(A,2X,'Processing data, file "',A,'" -->',A/
     +          3X,'Number of data points',12X,':',I6/
     +          3X,'Constant data background',9X,':',F5.2/
     +          3X,'detector signal',18X,': col',I3)
 2001 Format(A,2X,'Processing data, file "',A,'", scan',I4,' -->',A/
     +          3X,'Number of data points',12X,':',I6/
     +          3X,'Constant data background',9X,':',F5.2/
     +          3X,'detector signal',18X,': col',I3)
*
      If(ICE.ne.0) Then
       Write(6,2002) ICE
       Write(LUNPTL,2002) ICE
      Endif
 2002 Format(3X,'energy of x-rays',17X,': col',I3)
*
      If(ICA1.ne.0) Then
       Write(6,2003) ICA1
       Write(LUNPTL,2003) ICA1
      Endif
 2003 Format(3X,'1.crystal angle position         : col',I3)
*
      If(ICC1.ne.0) Then
       Write(6,2004) ICC1
       Write(LUNPTL,2003) ICC1
      Endif
 2004 Format(3X,'1.crystal angle calibration      : col',I3)
*
      If(ICT1.ne.0) Then
       Write(6,2005) ICT1
       Write(LUNPTL,2004) ICT1
      Endif
 2005 Format(3X,'1.crystal temperature correction : col',I3)
*
      If(ICA2.ne.0) Then
       Write(6,2006) ICA2
       Write(LUNPTL,2006) ICA2
      Endif
 2006 Format(3X,'2.crystal angle position         : col',I3)
*
      If(ICC2.ne.0) Then
       Write(6,2007) ICC2
       Write(LUNPTL,2007) ICC2
      Endif
 2007 Format(3X,'2.crystal angle calibration      : col',I3)
*
      If(ICT2.ne.0) Then
       Write(6,2008) ICT2
       Write(LUNPTL,2008) ICT2
      Endif
 2008 Format(3X,'2.crystal temperature correction : col',I3)
*
      If(ICT3.ne.0) Then
       Write(6,2009) ICT3
       Write(LUNPTL,2009) ICT3
      Endif
 2009 Format(3X,'analyzer crystal temp. corr.     : col',I3)
*
      If(ICM.ne.0) Then
       Write(6,2010) ICM
       Write(LUNPTL,2010) ICM
      Endif
 2010 Format(3X,'Monitor',26X,': col',I3)
*
      If(ICN.ne.0) Then
       Write(6,2011) ICN
       Write(LUNPTL,2011) ICN
      Endif
 2011 Format(3X,'Normalization',20X,': col',I3)
*
      If(BPEAK) Then
       If(ICP.eq.0) Then
        IC = ICD
       Else
        IC = ICP
       Endif
       Write(6,2012) IC
       Write(LUNPTL,2012) IC
      Else
       Write(6,2013) DENSHF
       Write(LUNPTL,2013) DENSHF
      Endif
 2012 Format(3X,'Peak position',20X,': col',I3)
 2013 Format(3X,'Peak position fixed at',11X,':',F8.2,' meV')
*
*:  remove noise spikes from data
*
      DEVN = DNSRM*Dsqrt(Dabs(DMZ(2) + DMZ(1)))
      DIFF = DMZ(1) - DMZ(2)
      If(DIFF.gt.DEVN) Then
       DMZ(1) = DMZ(2)
       KNP    = 1
      Else
       KNP = 0
      Endif
*
      DEVN = DNSRM*Dsqrt(Dabs(DMY(2) + DMY(1)))
      DIFF = DMY(1) - DMY(2)
      If(DIFF.gt.DEVN) DMY(1) = DMY(2)
*
      Do  K = 1,KD-2
       DEVN = Dabs(DMZ(K+1) + DMZ(K))
       DEVN = DNSRM*Dmax1(Dsqrt(DEVN),0.02D0*DEVN)
       DIF1 = DMZ(K+1) - DMZ(K)
       DIF2 = DMZ(K+1) - DMZ(K+2)
       If(DIF1.gt.DEVN .and. DIF2.gt.DEVN) Then
        DMZ(K+1) = 0.5D0*(DMZ(K) + DMZ(K+2))
        KNP      = KNP + 1
       Endif
*
       DEVN = Dabs(DMY(K+1) + DMY(K))
       DEVN = DNSRM*Dmax1(Dsqrt(DEVN),0.02D0*DEVN)
       DIF1 = DMY(K+1) - DMY(K)
       DIF2 = DMY(K+1) - DMY(K+2)
       If(DIF1.gt.DEVN .and. DIF2.gt.DEVN) Then
        DMY(K+1) = 0.5D0*(DMY(K) + DMY(K+2))
       Endif
      EndDo
*
      DEVN = DNSRM*Dsqrt(Dabs(DMZ(KD) + DMZ(KD-1)))
      DIFF = DMZ(KD) - DMZ(KD-1)
      If(DIFF.gt.DEVN) Then
       DMZ(KD) = DMZ(KD-1)
       KNP     = KNP + 1
      Endif
*
      DEVN = DNSRM*Dsqrt(Dabs(DMY(KD) + DMY(KD-1)))
      DIFF = DMY(KD) - DMY(KD-1)
      If(DIFF.gt.DEVN) DMY(KD) = DMY(KD-1)
*
      If(KNP.ne.0) Then
       Write(6,2020) KNP
       Write(LUNPTL,2020) KNP
      Endif
 2020 Format(3X,'Number of removed noise spikes   :',I4)
*
*:  calculate errors^2 from data
*
      Do  K = 1,KD
       DMDY(K) = Dmax1(1.D0,DMY(K))
       DMDZ(K) = DMZ(K)
      EndDo
*
*
      End

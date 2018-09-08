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
*
* purpose :  define parameters for PHOENIX modules
* date    :  April 9, 1996
* author  :  Wolfgang Sturhahn
*                                                                       
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
*:  generic variable type declarations
*:  ==================================
*
#include<implici.h>
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
*:  parameters that determine array dimensions
*:  ==========================================
*
*:  parameters give the upper limits of..
*
*:  PDS    =  no. of input data files
*:  PDATA  =  no. of data lines in each file
*:  PFIT   =  no. of fit parameters
*:  PMN    =  no. of moments
*:  PFFT   =  no. of elements in FFT routine
*:           (note: keep PFFT >= 2*PDATA and PFFT = 2**N)
*:  PLINES =  no. of lines in input file
*
      PARAMETER(PDS = 100, PDATA = 2**12, PFIT = 10, PMN = 3,
     +          PFFT = 4*PDATA, PLINES = 500)
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
*:  miscellaneous parameters PHOENIX modules PHOX and PADD
*:  ======================================================
*
*:  PCMAX  =  max. number of addressable cols. for formatted input files
*:  PITERA =  maximum number of iteration steps for fitting
*:              of the elastic peak
*:  DCVG   =  convergence criterium for fitting of the elastic peak
*:  XFORM* =  output format for * variables
*
      Character*11  XFORM2,XFORM3
      Character*20  XFORM2C
      Character*30  XFORM3C
*
      Parameter(PCMAX = 30, PITERA = 500, DCVG = 1.D-3,
     +          XFORM2  = '(1P,2E13.5)',XFORM3 = '(1P,3E13.5)',
     +          XFORM2C = '(1P,E12.5,'','',E12.5)',
     +          XFORM3C = '(1P,E12.5,'','',E12.5,'','',E12.5)')
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
*:  miscellaneous parameters PHOENIX module PHOX
*:  ============================================
*
*:  DSGLM1 =  %-confidence level for insignificant deviation
*:  DSGLM2 =  %-confidence level for acceptable deviation
*:  DBLIM  =  %-level for significant contribution of central peak
*
      Parameter(DSGLM1 = 63.D0, DSGLM2 = 99.D0, DBLIM = 20.D0)
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
*:  miscellaneous parameters PHOENIX module PADD
*:  ============================================
*
*:  PCLEN  =  max. length of lines for formatted input files (chars.)
*:  PSPEC  =  max. number of column specifiers for each input file
*:  DESCF  =  factor that converts energies in input to meV
*:  DASCF1 =  factor that converts angle1 in input to microrad
*:  DASCF2 =  factor that converts angle2 in input to microrad
*:  DCSCF  =  factor that converts calibration input to microrad
*:  DTSCF  =  factor that converts temperatures in input to K
*:  DNSRM  =  standard deviation to identify noise spikes
*
      Parameter(PCLEN = 500, PSPEC = 20, DESCF = 1.D6,
     +          DASCF1 = 1.D0, DASCF2 = 1.D0,
     +          DCSCF = 17453.29251994329D0, DTSCF = 1.D0,
     +          DNSRM = 5.D0)
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
*:  miscellaneous parameters PHOENIX module PSTH
*:  ============================================
*
*:  PTEMP  =  number of data points within temperature range
*:  PORDER =  max. order of phonon terms to phonon excitation prob.
*
      Parameter(PTEMP = 3001, PORDER = 20)
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
*:  miscellaneous parameters PHOENIX module PSVL
*:  ============================================
*
*:  PSVL  =  max. nr. of sound velocity fits
*
      Parameter(PSVL = 200)
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
*:  logical unit numbers
*:  ====================
*
*:   5       input,  standard input file
*:   6       output, standard printout, error messages
*:   LUNMES  input,  experimental data
*:   LUNINP  input,  control data
*:   LUNOUT  output, extended printout, experimental and
*:                                       calculated data
*:   LUNPIP  input/output, fifo pipe for visualization
*:   LUNTMP  input/output, temporary file
*
      Parameter(LUNMES =  1, LUNPTL =  2, LUNINP =  4, LUNOUT = 7,
     +          LUNPIP = 10, LUNTMP = 11)
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
*:  miscellaneous parameters
*:  ========================
*
*:   DRADNG  =  conversion from radian to degree
*:   DGNRAD  =  conversion from degree to radian
*:   DPIH    =  pi/2
*:   DPI     =  pi
*:   D2PI    =  2*pi
*:   DLN2    =  ln(2)
*:   DHQUER  =  (Planck constant)/2*pi    in units of eV*s
*:   DAMU    =  atomic mass constant      in units of g
*:   DCLIC   =  speed of light            in units of m/s
*:   DMUEK   =  nuclear magneton          in units of eV/T
*:   DCER0   =  classical electron radius in units of m
*:   DBLTZ   =  Boltzmann constant        in units of meV/K
*:   DELCH   =  elementary charge         in units of C
*
      Parameter(DRADNG = 5.729577951308232D1,
     +          DGNRAD = 1.745329251994329D-2,
     +          DPI4   = 7.853981633974483D-1,
     +          DPIH   = 1.570796326794897D0,
     +          DPI    = 3.141592653589793D0,
     +          D2PI   = 6.283185307179586D0,
     +          DLN2   = 6.931471805599453D-1,
     +          DSQ2   = 1.414213562373095D0,
     +          DHQUER = 6.582118890D-16,
     +          DAMU   = 1.66053873D-24,
     +          DCLIC  = 2.99792458D+08,
     +          DMUEK  = 3.152451238D-08,
     +          DCER0  = 2.817940285D-15,
     +          DBLTZ  = 8.617342D-2,
     +          DELCH  = 1.602176462D-19)
*

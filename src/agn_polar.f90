FUNCTION AGN_POLAR(lam,spec,pset,lbol_csp)

  USE sps_vars; USE sps_utils, ONLY: locate,attn_curve
  IMPLICIT NONE

  REAL(SP), DIMENSION(nspec), INTENT(in) :: lam,spec
  REAL(SP), INTENT(in)       :: lbol_csp
  TYPE(PARAMS), INTENT(in)   :: pset
  REAL(SP), DIMENSION(nspec) :: agn_polar
  INTEGER  :: jlo
  REAL(SP) :: dj
 
  !--------------------------------------------------------------!

  !attenuate the AGN emission by the diffuse dust
!  agnspeci = agnspeci*EXP(-attn_curve(spec_lambda,dust_type,pset))

!  write(6,*)'In agn polar',pset%fagnp* agnincl_polar
  agn_polar = spec + 10**lbol_csp*pset%fagnp*agnincl_polar
  
END FUNCTION AGN_POLAR
  

FUNCTION AGN_INCLINATION_POLAR(lam,spec,pset,lbol_csp)

  USE sps_vars; USE sps_utils, ONLY: locate,attn_curve
  IMPLICIT NONE

  REAL(SP), DIMENSION(nspec), INTENT(in) :: lam,spec
  REAL(SP), INTENT(in)       :: lbol_csp
  TYPE(PARAMS), INTENT(in)   :: pset
  REAL(SP), DIMENSION(nspec) :: agn_inclination_polar,agnspeci
  INTEGER  :: jlo
  REAL(SP) :: dj
 
  !--------------------------------------------------------------!

  ! WRITE(6,*) 'Calling agn_inclination,',pset%agn_incl,pset%fagni,pset%fagnp
  !interpolate in ang_incl
  
  jlo = MIN(MAX(locate(agnincl_values,pset%agn_incl),1),&
       nagnincl-1)
  dj  = (pset%agn_incl-agnincl_values(jlo)) / &
       (agnincl_values(jlo+1)-agnincl_values(jlo))
  dj  = MAX(MIN(dj,1.0),0.0) !no extrapolation

  !  write(6,*) 'jlo,dj,dust',jlo,dj,dust_type
  ! interpolated value of agn spec for given pset%ang_incl
  agnspeci  = (1-dj)*agnincl_spec(:,jlo) + dj*agnincl_spec(:,jlo+1)

  !attenuate the AGN emission by the diffuse dust
  agnspeci = agnspeci*EXP(-attn_curve(spec_lambda,dust_type,pset))

  !  agn_inclination = spec + 10**lbol_csp*pset%fagni*(agnspeci)
 
  agn_inclination_polar = spec + 10**lbol_csp*pset%fagni*(agnspeci + pset%fagnp* agnincl_polar)
  
END FUNCTION AGN_INCLINATION_POLAR
  

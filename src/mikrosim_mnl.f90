program mikrosim_mnl
  implicit none

  integer, parameter :: nobs = 8, nalts = 3
  integer :: i, j
  real(8) :: b1, b2, b3, b4
  real(8), dimension(nobs) :: purchase_cost, fuel_cost, curb_weight
  real(8), dimension(nalts) :: utility, exp_utility, probability
  real(8) :: denominator

  purchase_cost = (/ 100.0d0, 120.0d0, 140.0d0, 160.0d0, 180.0d0, 200.0d0, 300.0d0, 400.0d0 /)
  fuel_cost = (/ 6.0d0, 8.0d0, 7.0d0, 4.0d0, 9.0d0, 8.0d0, 14.0d0, 16.0d0 /)
  curb_weight = (/ 1200.0d0, 1300.0d0, 1600.0d0, 1800.0d0, 2300.0d0, 1700.0d0, 1900.0d0, 2000.0d0 /)

  print *, 'Enter model parameters b1, b2, b3, b4:'
  read (*,*) b1, b2, b3, b4

  print *
  print '(A)', 'Multinomial logit probabilities for 3 alternatives'
  print '(A)', '------------------------------------------------'
  print '(A)', 'Row   P(alt1)      P(alt2)      P(alt3)'

  do i = 1, nobs
    do j = 1, nalts
      utility(j) = b1 + b2*dble(j) + b3*purchase_cost(i) + b4*fuel_cost(i) - 0.0005d0*curb_weight(i)
      exp_utility(j) = exp(utility(j))
    end do

    denominator = sum(exp_utility)
    probability = exp_utility / denominator

    write (*,'(I3,3F13.6)') i, probability(1), probability(2), probability(3)
  end do

end program mikrosim_mnl

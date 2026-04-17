module microsim_types
  implicit none
  integer, parameter :: dp = kind(1.0d0)
  integer, parameter :: max_households = 100000
  integer, parameter :: max_zones = 5000
  integer, parameter :: max_cars = 10000
  integer, parameter :: n_fuel = 10
  integer, parameter :: n_income = 5
  integer, parameter :: n_make = 32

  type :: household_t
    integer :: zone_number = 0
    integer :: record_number = 0
    integer :: household_id = 0
    integer :: person_id = 0
    real(dp) :: hh_bost = 0.0_dp
    real(dp) :: hh_ink = 0.0_dp
    real(dp) :: hh_n_arb = 0.0_dp
    real(dp) :: hh_n_bil = 0.0_dp
    real(dp) :: hh_n_kk = 0.0_dp
    real(dp) :: hh_typ = 0.0_dp
    real(dp) :: kort = 0.0_dp
    real(dp) :: p0_age = 0.0_dp
    real(dp) :: p0_forv = 0.0_dp
    real(dp) :: p0_ink = 0.0_dp
    real(dp) :: p0_kk = 0.0_dp
    real(dp) :: p0_kort = 0.0_dp
    real(dp) :: p0_sex = 0.0_dp
    real(dp) :: sample_hh_id = 0.0_dp
    real(dp) :: weight = 1.0_dp
    real(dp) :: ant_vux = 0.0_dp
    real(dp) :: ant_barn = 0.0_dp
    real(dp) :: hhvikt = 1.0_dp
  end type household_t

  type :: zone_t
    integer :: zone_number = 0
    real(dp) :: detached_house = 0.0_dp
    real(dp) :: grant = 0.0_dp
    real(dp) :: public_charging_home = 0.0_dp
    real(dp) :: public_charging_work = 0.0_dp
    real(dp) :: rural_area = 0.0_dp
    real(dp) :: small_city = 0.0_dp
  end type zone_t

  type :: car_t
    integer :: choice_ut = 0
    integer :: lopnr = 0
    integer :: altnr = 0
    integer :: make = 0
    integer :: fueltype = 0
    integer :: fclass = 0
    real(dp) :: kombi = 0.0_dp
    real(dp) :: rust = 0.0_dp
    real(dp) :: curbw = 0.0_dp
    real(dp) :: lprice = 0.0_dp
    real(dp) :: nco9 = 0.0_dp
    real(dp) :: ncc09 = 0.0_dp
    real(dp) :: rangee = 0.0_dp
    real(dp) :: fuelcons = 0.0_dp
    real(dp) :: fuelcons_el = 0.0_dp
    real(dp) :: segut = 0.0_dp
    integer :: storlekut = 0
    integer :: karossut = 0
    real(dp) :: co2 = 0.0_dp
  end type car_t

  type :: scenario_t
    real(dp) :: fossil_consumption_factor = 1.0_dp
    real(dp) :: electric_range_factor = 1.0_dp
    real(dp) :: price_factor = 1.0_dp
    real(dp) :: tax_factor = 1.0_dp
  end type scenario_t

  type :: param_t
    real(dp) :: beta_ln_ink
    real(dp) :: beta_antpers
    real(dp) :: beta_inkomst
    real(dp) :: beta_hustyp
    real(dp) :: beta_kk_ant
    real(dp) :: beta_ejnykonst
    real(dp) :: beta_price_hi(n_income)
    real(dp) :: beta_opcost
    real(dp) :: beta_rust
    real(dp) :: beta_passenger_safety
    real(dp) :: beta_safety_systems
    real(dp) :: beta_size_small
    real(dp) :: beta_size_mid
    real(dp) :: beta_size_large
    real(dp) :: beta_size_sport
    real(dp) :: beta_petrol
    real(dp) :: beta_phev_diesel
    real(dp) :: beta_phev_petrol
    real(dp) :: beta_e85
    real(dp) :: beta_gas
    real(dp) :: beta_hev_diesel
    real(dp) :: beta_hev_petrol
    real(dp) :: beta_bev
    real(dp) :: beta_diesel
    real(dp) :: beta_many_cars_hh
    real(dp) :: beta_detached_house
    real(dp) :: beta_grant
    real(dp) :: beta_public_charging_home
    real(dp) :: beta_public_charging_work
    real(dp) :: beta_rural_area
    real(dp) :: beta_small_city
    real(dp) :: beta_make(n_make)
  end type param_t

contains

  pure function fuel_in_scope(fueltype) result(in_scope)
    integer, intent(in) :: fueltype
    logical :: in_scope
    in_scope = (fueltype == 3 .or. fueltype == 9 .or. fueltype == 10)
  end function fuel_in_scope

  pure function fossil_fuel(fueltype) result(is_fossil)
    integer, intent(in) :: fueltype
    logical :: is_fossil
    is_fossil = (fueltype == 1 .or. fueltype == 2 .or. fueltype == 5 .or. fueltype == 6 .or. &
                 fueltype == 7 .or. fueltype == 8 .or. fueltype == 9 .or. fueltype == 10)
  end function fossil_fuel

end module microsim_types

module microsim_model
  use microsim_types
  implicit none
contains

  subroutine init_parameters(p)
    type(param_t), intent(out) :: p

    p%beta_ln_ink = -0.736346925665d-01
    p%beta_antpers = 0.412722645306d0
    p%beta_inkomst = 0.117909204811d-02
    p%beta_hustyp = -0.450683316579d0
    p%beta_kk_ant = 1.50214051486d0
    p%beta_ejnykonst = 4.93231811081d0

    p%beta_price_hi = (/ -0.01d0, -0.0067d0, -0.0038d0, -0.0018d0, 0.00025d0 /)
    p%beta_opcost = -0.052d0
    p%beta_rust = 0.506d0
    p%beta_passenger_safety = 0.005d0
    p%beta_safety_systems = 0.008d0
    p%beta_size_small = 0.0d0
    p%beta_size_mid = 0.783d0
    p%beta_size_large = 1.112d0
    p%beta_size_sport = -0.568d0
    p%beta_petrol = 0.0d0
    p%beta_phev_diesel = -4.008d0
    p%beta_phev_petrol = -2.635d0
    p%beta_e85 = -0.432d0
    p%beta_gas = -3.946d0
    p%beta_hev_diesel = -1.144d0
    p%beta_hev_petrol = -0.515d0
    p%beta_bev = -2.158d0
    p%beta_diesel = -1.087d0
    p%beta_many_cars_hh = 0.272d0
    p%beta_detached_house = 0.614d0
    p%beta_grant = 0.477d0
    p%beta_public_charging_home = 0.022d0
    p%beta_public_charging_work = 0.011d0
    p%beta_rural_area = 0.0d0
    p%beta_small_city = -0.38d0

    p%beta_make = (/ &
      3.205d0, 3.345d0, 3.050d0, 2.712d0, 2.220d0, 2.753d0, 2.365d0, -7.305d0, &
      2.073d0, 1.578d0, 1.783d0, 1.718d0, 4.517d0, 1.646d0, 1.016d0, 1.721d0, &
      1.209d0, 3.790d0, 2.611d0, 0.513d0, 1.830d0, 0.729d0, -0.055d0, 1.404d0, &
      -0.368d0, 3.873d0, 2.305d0, 3.952d0, 4.379d0, 3.000d0, -0.721d0, 0.000d0 /)
  end subroutine init_parameters

  pure integer function income_class(ink_thousand)
    real(dp), intent(in) :: ink_thousand
    if (ink_thousand <= 400.0_dp) then
      income_class = 1
    else if (ink_thousand <= 800.0_dp) then
      income_class = 2
    else if (ink_thousand <= 1600.0_dp) then
      income_class = 3
    else if (ink_thousand <= 2400.0_dp) then
      income_class = 4
    else
      income_class = 5
    end if
  end function income_class

  pure real(dp) function safe_log(x)
    real(dp), intent(in) :: x
    safe_log = log(max(x, 1.0d-9))
  end function safe_log

  pure real(dp) function circulation_tax(fueltype, co2, tax_factor)
    integer, intent(in) :: fueltype
    real(dp), intent(in) :: co2, tax_factor
    real(dp) :: base_tax

    base_tax = 0.0_dp
    if (fueltype == 1 .or. fueltype == 5) then
      base_tax = 360.0_dp + 107.0_dp * min(max(co2 - 75.0_dp, 0.0_dp), 50.0_dp) + 132.0_dp * max(co2 - 125.0_dp, 0.0_dp)
    else if (fueltype == 2 .or. fueltype == 6) then
      base_tax = 360.0_dp + 107.0_dp * min(max(co2 - 75.0_dp, 0.0_dp), 50.0_dp) + 132.0_dp * max(co2 - 125.0_dp, 0.0_dp) + &
                 co2 * 13.52_dp + 250.0_dp
    end if
    circulation_tax = base_tax * tax_factor
  end function circulation_tax

  pure real(dp) function operational_cost(car, fuel_prices, tax_factor)
    type(car_t), intent(in) :: car
    real(dp), intent(in) :: fuel_prices(n_fuel)
    real(dp), intent(in) :: tax_factor
    real(dp) :: fp

    fp = 0.0_dp
    if (car%fueltype >= 1 .and. car%fueltype <= n_fuel) fp = fuel_prices(car%fueltype)

    if (car%fueltype == 3) then
      operational_cost = car%fuelcons * 150.0_dp * fp + circulation_tax(car%fueltype, car%co2, tax_factor)
    else
      operational_cost = car%fuelcons_el * 150.0_dp * fp + circulation_tax(car%fueltype, car%co2, tax_factor)
    end if
  end function operational_cost

  pure real(dp) function new_car_probability(hh, p)
    type(household_t), intent(in) :: hh
    type(param_t), intent(in) :: p
    real(dp) :: v_buy, v_not
    real(dp) :: ln_ink, antpers, inkomst, hustyp, kk_ant

    ln_ink = safe_log(hh%hh_ink / 1000.0_dp)
    antpers = hh%ant_vux + hh%ant_barn
    inkomst = hh%hh_ink / 1000.0_dp
    hustyp = hh%hh_bost
    kk_ant = hh%hh_n_kk

    v_buy = p%beta_ln_ink * ln_ink + p%beta_antpers * antpers + p%beta_inkomst * inkomst + &
            p%beta_hustyp * hustyp + p%beta_kk_ant * kk_ant
    v_not = p%beta_ejnykonst

    if (v_not - v_buy > 40.0_dp) then
      new_car_probability = 0.0_dp
    else if (v_not - v_buy < -40.0_dp) then
      new_car_probability = 1.0_dp
    else
      new_car_probability = 1.0_dp / (1.0_dp + exp(v_not - v_buy))
    end if
  end function new_car_probability

  pure real(dp) function car_utility(car, hh, z, p, inc_class, fuel_prices, scenario)
    type(car_t), intent(in) :: car
    type(household_t), intent(in) :: hh
    type(zone_t), intent(in) :: z
    type(param_t), intent(in) :: p
    integer, intent(in) :: inc_class
    real(dp), intent(in) :: fuel_prices(n_fuel)
    type(scenario_t), intent(in) :: scenario

    real(dp) :: many_cars
    real(dp) :: size_small, size_mid, size_large, size_sport

    car_utility = 0.0_dp

    car_utility = car_utility + p%beta_price_hi(inc_class) * car%lprice
    car_utility = car_utility + p%beta_opcost * operational_cost(car, fuel_prices, scenario%tax_factor)
    car_utility = car_utility + p%beta_rust * car%rust
    car_utility = car_utility + p%beta_passenger_safety * car%nco9
    car_utility = car_utility + p%beta_safety_systems * car%ncc09

    size_small = 0.0_dp
    size_mid = 0.0_dp
    size_large = 0.0_dp
    size_sport = 0.0_dp

    if (car%storlekut == 1) size_small = 1.0_dp
    if (car%storlekut == 2 .or. car%storlekut == 3) size_mid = 1.0_dp
    if (car%storlekut == 4 .or. car%storlekut == 5) size_large = 1.0_dp
    if (car%storlekut == 6 .or. car%storlekut == 7) size_sport = 1.0_dp

    car_utility = car_utility + p%beta_size_small * size_small
    car_utility = car_utility + p%beta_size_mid * size_mid
    car_utility = car_utility + p%beta_size_large * size_large
    car_utility = car_utility + p%beta_size_sport * size_sport

    select case (car%fueltype)
      case (1)
        car_utility = car_utility + p%beta_petrol
      case (2)
        car_utility = car_utility + p%beta_diesel
      case (3)
        car_utility = car_utility + p%beta_bev
      case (5)
        car_utility = car_utility + p%beta_hev_petrol
      case (6)
        car_utility = car_utility + p%beta_hev_diesel
      case (7)
        car_utility = car_utility + p%beta_gas
      case (8)
        car_utility = car_utility + p%beta_e85
      case (9)
        car_utility = car_utility + p%beta_phev_petrol
      case (10)
        car_utility = car_utility + p%beta_phev_diesel
    end select

    many_cars = 0.0_dp
    if (fuel_in_scope(car%fueltype) .and. hh%hh_n_bil > 1.0_dp) many_cars = 1.0_dp
    car_utility = car_utility + p%beta_many_cars_hh * many_cars

    if (fuel_in_scope(car%fueltype)) then
      car_utility = car_utility + p%beta_detached_house * z%detached_house
      car_utility = car_utility + p%beta_grant * z%grant
      car_utility = car_utility + p%beta_public_charging_home * z%public_charging_home
      car_utility = car_utility + p%beta_public_charging_work * z%public_charging_work
      car_utility = car_utility + p%beta_rural_area * z%rural_area
      car_utility = car_utility + p%beta_small_city * z%small_city
    end if

    if (car%make >= 1 .and. car%make <= n_make) then
      car_utility = car_utility + p%beta_make(car%make)
    end if
  end function car_utility

  subroutine softmax(utilities, n, probabilities)
    integer, intent(in) :: n
    real(dp), intent(in) :: utilities(n)
    real(dp), intent(out) :: probabilities(n)
    real(dp) :: max_u, denom
    integer :: i

    max_u = maxval(utilities)
    do i = 1, n
      probabilities(i) = exp(min(700.0_dp, utilities(i) - max_u))
    end do

    denom = sum(probabilities)
    if (denom <= 0.0_dp) then
      probabilities = 1.0_dp / real(n, dp)
    else
      probabilities = probabilities / denom
    end if
  end subroutine softmax

  subroutine apply_scenario(cars, n_cars, scenario)
    type(car_t), intent(inout) :: cars(:)
    integer, intent(in) :: n_cars
    type(scenario_t), intent(in) :: scenario
    integer :: i

    do i = 1, n_cars
      if (fossil_fuel(cars(i)%fueltype)) cars(i)%fuelcons_el = cars(i)%fuelcons_el * scenario%fossil_consumption_factor
      if (fuel_in_scope(cars(i)%fueltype)) cars(i)%rangee = cars(i)%rangee * scenario%electric_range_factor
      cars(i)%lprice = cars(i)%lprice * scenario%price_factor
    end do
  end subroutine apply_scenario

end module microsim_model

module microsim_io
  use microsim_types
  implicit none
contains

  subroutine normalize_line(line)
    character(len=*), intent(inout) :: line
    integer :: i
    do i = 1, len_trim(line)
      if (line(i:i) == ',' .or. line(i:i) == ';' .or. iachar(line(i:i)) == 9) line(i:i) = ' '
    end do
  end subroutine normalize_line

  pure function to_upper(text) result(out)
    character(len=*), intent(in) :: text
    character(len=len(text)) :: out
    integer :: i, c

    out = text
    do i = 1, len(text)
      c = iachar(text(i:i))
      if (c >= iachar('a') .and. c <= iachar('z')) out(i:i) = achar(c - 32)
    end do
  end function to_upper

  subroutine read_population(filename, households, n_households)
    character(len=*), intent(in) :: filename
    type(household_t), intent(out) :: households(:)
    integer, intent(out) :: n_households

    integer :: ios, unit_no
    character(len=1000) :: line
    real(dp) :: values(22)

    n_households = 0
    unit_no = 10
    open(unit=unit_no, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') 'Could not open population file: ' // trim(filename)
      stop 1
    end if

    do
      read(unit_no, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (len_trim(line) == 0) cycle

      call normalize_line(line)
      read(line, *, iostat=ios) values
      if (ios /= 0) cycle

      n_households = n_households + 1
      if (n_households > size(households)) then
        write(*,'(A)') 'Population exceeds allocated maximum.'
        stop 1
      end if

      households(n_households)%zone_number = int(values(1))
      households(n_households)%record_number = int(values(2))
      households(n_households)%household_id = int(values(3))
      households(n_households)%person_id = int(values(4))
      households(n_households)%hh_bost = values(5)
      households(n_households)%hh_ink = values(6)
      households(n_households)%hh_n_arb = values(7)
      households(n_households)%hh_n_bil = values(8)
      households(n_households)%hh_n_kk = values(9)
      households(n_households)%hh_typ = values(10)
      households(n_households)%kort = values(11)
      households(n_households)%p0_age = values(12)
      households(n_households)%p0_forv = values(13)
      households(n_households)%p0_ink = values(14)
      households(n_households)%p0_kk = values(15)
      households(n_households)%p0_kort = values(16)
      households(n_households)%p0_sex = values(17)
      households(n_households)%sample_hh_id = values(18)
      households(n_households)%weight = values(19)
      households(n_households)%ant_vux = values(20)
      households(n_households)%ant_barn = values(21)
      households(n_households)%hhvikt = values(22)
    end do

    close(unit_no)
  end subroutine read_population

  subroutine read_zones(filename, zones, n_zones)
    character(len=*), intent(in) :: filename
    type(zone_t), intent(out) :: zones(:)
    integer, intent(out) :: n_zones

    integer :: ios, unit_no
    character(len=1000) :: line
    real(dp) :: values(7)

    n_zones = 0
    unit_no = 11
    open(unit=unit_no, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') 'Could not open zone file: ' // trim(filename)
      stop 1
    end if

    do
      read(unit_no, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (len_trim(line) == 0) cycle

      call normalize_line(line)
      read(line, *, iostat=ios) values
      if (ios /= 0) cycle

      n_zones = n_zones + 1
      if (n_zones > size(zones)) then
        write(*,'(A)') 'Zone list exceeds allocated maximum.'
        stop 1
      end if

      zones(n_zones)%zone_number = int(values(1))
      zones(n_zones)%detached_house = values(2)
      zones(n_zones)%grant = values(3)
      zones(n_zones)%public_charging_home = values(4)
      zones(n_zones)%public_charging_work = values(5)
      zones(n_zones)%rural_area = values(6)
      zones(n_zones)%small_city = values(7)
    end do

    close(unit_no)
  end subroutine read_zones

  subroutine read_cars(filename, cars, n_cars)
    character(len=*), intent(in) :: filename
    type(car_t), intent(out) :: cars(:)
    integer, intent(out) :: n_cars

    integer :: ios, unit_no
    character(len=1000) :: line
    real(dp) :: values(19)

    n_cars = 0
    unit_no = 12
    open(unit=unit_no, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') 'Could not open car alternatives file: ' // trim(filename)
      stop 1
    end if

    do
      read(unit_no, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (len_trim(line) == 0) cycle

      call normalize_line(line)
      read(line, *, iostat=ios) values
      if (ios /= 0) cycle

      n_cars = n_cars + 1
      if (n_cars > size(cars)) then
        write(*,'(A)') 'Car alternatives exceed allocated maximum.'
        stop 1
      end if

      cars(n_cars)%choice_ut = int(values(1))
      cars(n_cars)%lopnr = int(values(2))
      cars(n_cars)%altnr = int(values(3))
      cars(n_cars)%make = int(values(4))
      cars(n_cars)%fueltype = int(values(5))
      cars(n_cars)%fclass = int(values(6))
      cars(n_cars)%kombi = values(7)
      cars(n_cars)%rust = values(8)
      cars(n_cars)%curbw = values(9)
      cars(n_cars)%lprice = values(10)
      cars(n_cars)%nco9 = values(11)
      cars(n_cars)%ncc09 = values(12)
      cars(n_cars)%rangee = values(13)
      cars(n_cars)%fuelcons = values(14)
      cars(n_cars)%fuelcons_el = values(15)
      cars(n_cars)%segut = values(16)
      cars(n_cars)%storlekut = int(values(17))
      cars(n_cars)%karossut = int(values(18))
      cars(n_cars)%co2 = values(19)
    end do

    close(unit_no)
  end subroutine read_cars

  subroutine read_fuel_prices(filename, fuel_prices)
    character(len=*), intent(in) :: filename
    real(dp), intent(out) :: fuel_prices(n_fuel)

    integer :: ios, unit_no, fuel
    character(len=1000) :: line
    real(dp) :: price

    fuel_prices = (/ 23.0_dp, 24.0_dp, 2.3_dp, 0.0_dp, 23.5_dp, 24.5_dp, 18.0_dp, 19.5_dp, 22.0_dp, 23.0_dp /)

    unit_no = 13
    open(unit=unit_no, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) return

    do
      read(unit_no, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (len_trim(line) == 0) cycle

      call normalize_line(line)
      read(line, *, iostat=ios) fuel, price
      if (ios /= 0) cycle
      if (fuel >= 1 .and. fuel <= n_fuel) fuel_prices(fuel) = price
    end do

    close(unit_no)
  end subroutine read_fuel_prices

  subroutine read_scenario(filename, scenario)
    character(len=*), intent(in) :: filename
    type(scenario_t), intent(inout) :: scenario

    integer :: ios, unit_no
    character(len=1000) :: line
    character(len=64) :: key
    real(dp) :: value

    unit_no = 14
    open(unit=unit_no, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) return

    do
      read(unit_no, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (len_trim(line) == 0) cycle

      call normalize_line(line)
      read(line, *, iostat=ios) key, value
      if (ios /= 0) cycle

      select case (trim(to_upper(key)))
        case ('FOSSIL_CONSUMPTION_FACTOR')
          scenario%fossil_consumption_factor = value
        case ('ELECTRIC_RANGE_FACTOR')
          scenario%electric_range_factor = value
        case ('PRICE_FACTOR')
          scenario%price_factor = value
        case ('TAX_FACTOR')
          scenario%tax_factor = value
      end select
    end do

    close(unit_no)
  end subroutine read_scenario

  pure integer function zone_index(zone_number, zones, n_zones)
    integer, intent(in) :: zone_number, n_zones
    type(zone_t), intent(in) :: zones(:)
    integer :: i

    zone_index = 0
    do i = 1, n_zones
      if (zones(i)%zone_number == zone_number) then
        zone_index = i
        return
      end if
    end do
  end function zone_index

  subroutine write_alternative_output(filename, cars, n_cars, alt_demand)
    character(len=*), intent(in) :: filename
    type(car_t), intent(in) :: cars(:)
    integer, intent(in) :: n_cars
    real(dp), intent(in) :: alt_demand(:)

    integer :: unit_no, i

    unit_no = 20
    open(unit=unit_no, file=filename, status='replace', action='write')
    write(unit_no,'(A)') 'Choice_ut,Lopnr,Make,Fueltype,Fclass,ExpectedDemand'
    do i = 1, n_cars
      write(unit_no,'(I0,A,I0,A,I0,A,I0,A,I0,A,F12.6)') cars(i)%choice_ut, ',', cars(i)%lopnr, ',', cars(i)%make, ',', &
          cars(i)%fueltype, ',', cars(i)%fclass, ',', alt_demand(i)
    end do
    close(unit_no)
  end subroutine write_alternative_output

  subroutine write_national_output(filename, national)
    character(len=*), intent(in) :: filename
    real(dp), intent(in) :: national(n_fuel, n_income)

    integer :: unit_no, f, h

    unit_no = 21
    open(unit=unit_no, file=filename, status='replace', action='write')
    write(unit_no,'(A)') 'Fueltype,IncomeClass,ExpectedDemand'
    do f = 1, n_fuel
      do h = 1, n_income
        write(unit_no,'(I0,A,A,A,F12.6)') f, ',', 'HI' // trim(adjustl(itoa(h))), ',', national(f, h)
      end do
    end do
    close(unit_no)
  contains
    pure function itoa(i) result(s)
      integer, intent(in) :: i
      character(len=16) :: s
      write(s,'(I0)') i
    end function itoa
  end subroutine write_national_output

  subroutine write_zonal_output(filename, zones, n_zones, zonal)
    character(len=*), intent(in) :: filename
    type(zone_t), intent(in) :: zones(:)
    integer, intent(in) :: n_zones
    real(dp), intent(in) :: zonal(n_zones, n_fuel, n_income)

    integer :: unit_no, z, f, h

    unit_no = 22
    open(unit=unit_no, file=filename, status='replace', action='write')
    write(unit_no,'(A)') 'Zone,Fueltype,IncomeClass,ExpectedDemand'
    do z = 1, n_zones
      do f = 1, n_fuel
        do h = 1, n_income
          write(unit_no,'(I0,A,I0,A,A,A,F12.6)') zones(z)%zone_number, ',', f, ',', &
              'HI' // trim(adjustl(itoa(h))), ',', zonal(z, f, h)
        end do
      end do
    end do
    close(unit_no)
  contains
    pure function itoa(i) result(s)
      integer, intent(in) :: i
      character(len=16) :: s
      write(s,'(I0)') i
    end function itoa
  end subroutine write_zonal_output

end module microsim_io

program Mikrosim_ny
  use microsim_types
  use microsim_model
  use microsim_io
  implicit none

  type(household_t), allocatable :: households(:)
  type(zone_t), allocatable :: zones(:)
  type(car_t), allocatable :: cars(:)

  type(param_t) :: params
  type(scenario_t) :: scenario

  real(dp), allocatable :: utilities(:), probs(:), alt_demand(:)
  real(dp), allocatable :: national(:,:), zonal(:,:,:)
  real(dp) :: fuel_prices(n_fuel)

  integer :: n_households, n_zones, n_cars
  integer :: i, j, zidx, inc_class, fuel_idx
  real(dp) :: p_new, weighted_demand

  character(len=256) :: population_file, zones_file, cars_file, fuel_file, scenario_file, out_dir

  population_file = 'data/population.csv'
  zones_file = 'data/zones.csv'
  cars_file = 'data/base_car_alternatives.csv'
  fuel_file = 'data/Fuelprice.dat'
  scenario_file = 'data/scenario.dat'
  out_dir = 'output'

  call init_parameters(params)

  allocate(households(max_households))
  allocate(zones(max_zones))
  allocate(cars(max_cars))

  call read_population(population_file, households, n_households)
  call read_zones(zones_file, zones, n_zones)
  call read_cars(cars_file, cars, n_cars)
  call read_fuel_prices(fuel_file, fuel_prices)
  call read_scenario(scenario_file, scenario)

  if (n_households <= 0) then
    write(*,'(A)') 'No households loaded. Exiting.'
    stop 1
  end if
  if (n_zones <= 0) then
    write(*,'(A)') 'No zones loaded. Exiting.'
    stop 1
  end if
  if (n_cars <= 0) then
    write(*,'(A)') 'No car alternatives loaded. Exiting.'
    stop 1
  end if

  call apply_scenario(cars, n_cars, scenario)

  allocate(utilities(n_cars), probs(n_cars), alt_demand(n_cars))
  allocate(national(n_fuel, n_income))
  allocate(zonal(n_zones, n_fuel, n_income))

  alt_demand = 0.0_dp
  national = 0.0_dp
  zonal = 0.0_dp

  do i = 1, n_households
    zidx = zone_index(households(i)%zone_number, zones, n_zones)
    if (zidx == 0) cycle

    inc_class = income_class(households(i)%hh_ink / 1000.0_dp)
    p_new = new_car_probability(households(i), params)

    do j = 1, n_cars
      utilities(j) = car_utility(cars(j), households(i), zones(zidx), params, inc_class, fuel_prices, scenario)
    end do

    call softmax(utilities, n_cars, probs)

    do j = 1, n_cars
      weighted_demand = households(i)%weight * households(i)%hhvikt * p_new * probs(j)
      alt_demand(j) = alt_demand(j) + weighted_demand

      fuel_idx = cars(j)%fueltype
      if (fuel_idx < 1 .or. fuel_idx > n_fuel) cycle

      national(fuel_idx, inc_class) = national(fuel_idx, inc_class) + weighted_demand
      zonal(zidx, fuel_idx, inc_class) = zonal(zidx, fuel_idx, inc_class) + weighted_demand
    end do
  end do

  call execute_command_line('mkdir -p ' // trim(out_dir))
  call write_alternative_output(trim(out_dir) // '/alternative_demand.csv', cars, n_cars, alt_demand)
  call write_national_output(trim(out_dir) // '/national_fuel_income.csv', national)
  call write_zonal_output(trim(out_dir) // '/zonal_fuel_income.csv', zones, n_zones, zonal)

  write(*,'(A,I0)') 'Households processed: ', n_households
  write(*,'(A,I0)') 'Zones loaded: ', n_zones
  write(*,'(A,I0)') 'Car alternatives loaded: ', n_cars
  write(*,'(A)') 'Outputs written to output/alternative_demand.csv, output/national_fuel_income.csv, output/zonal_fuel_income.csv'

end program Mikrosim_ny

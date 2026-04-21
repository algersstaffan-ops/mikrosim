module microsim_kinds
  use, intrinsic :: iso_fortran_env, only: real64, int64
  implicit none

  integer, parameter :: dp = real64
  integer(int64), parameter :: EMPTY_KEY = huge(0_int64)

  integer, parameter :: MAX_POP_ROWS = 100000
  integer, parameter :: MAX_HOUSEHOLDS = 11000000
  integer, parameter :: MAX_ZONES = 100
  integer, parameter :: MAX_CARS = 600

  integer, parameter :: N_FUEL_TYPES = 10
  integer, parameter :: N_INCOME_CLASSES = 5
  integer, parameter :: N_OUTPUT_INCOME = 4
  integer, parameter :: N_MAKES = 32
end module microsim_kinds

module microsim_types
  use microsim_kinds
  implicit none

  type :: Household
    integer :: zone_number = 0
    integer(int64) :: household_id = 0_int64
    integer :: hh_bost = 0
    real(dp) :: hh_ink = 0.0_dp
    integer :: hh_n_bil = 0
    integer :: hh_n_kk = 0
    real(dp) :: ant_vux = 0.0_dp
    real(dp) :: ant_barn = 0.0_dp
    real(dp) :: hhvikt = 1.0_dp
  end type Household

  type :: ZoneDatum
    integer :: zone_number = 0
    real(dp) :: detached_house = 0.0_dp
    real(dp) :: grant = 0.0_dp
    real(dp) :: public_charging_home = 0.0_dp
    real(dp) :: public_charging_work = 0.0_dp
    real(dp) :: rural_area = 0.0_dp
    real(dp) :: small_city = 0.0_dp
  end type ZoneDatum

  type :: CarAlternative
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
    integer :: segut = 0
    integer :: storlekut = 0
    integer :: karossut = 0
    real(dp) :: co2 = 0.0_dp
  end type CarAlternative

  type :: ScenarioSettings
    real(dp) :: fossil_consumption_factor = 1.0_dp
    real(dp) :: electric_consumption_factor = 1.0_dp
    real(dp) :: ev_range_factor = 1.0_dp
    real(dp) :: purchase_price_factor = 1.0_dp
    real(dp) :: purchase_price_add_ksek = 0.0_dp
    real(dp) :: co2_factor = 1.0_dp
    real(dp) :: tax_multiplier = 1.0_dp
    real(dp) :: grant_multiplier = 1.0_dp
  end type ScenarioSettings

  type :: ModelParameters
    real(dp) :: buy_ln_ink = -0.736346925665E-01_dp
    real(dp) :: buy_antpers = 0.412722645306_dp
    real(dp) :: buy_inkomst = 0.117909204811E-02_dp
    real(dp) :: buy_hustyp = -0.450683316579_dp
    real(dp) :: buy_kk_ant = 1.50214051486_dp
    real(dp) :: no_buy_constant = 4.93231811081_dp

    real(dp) :: price_hi(N_INCOME_CLASSES) = [ &
      -0.01_dp, -0.0067_dp, -0.0038_dp, -0.0018_dp, 0.00025_dp ]
    real(dp) :: op_cost = -0.052_dp
    real(dp) :: rust_guarantee = 0.506_dp
    real(dp) :: passenger_safety = 0.005_dp
    real(dp) :: safety_systems = 0.008_dp
    real(dp) :: size_small = 0.0_dp
    real(dp) :: size_mid = 0.783_dp
    real(dp) :: size_large = 1.112_dp
    real(dp) :: size_sport = -0.568_dp
    real(dp) :: petrol = 0.0_dp
    real(dp) :: phev_diesel = -4.008_dp
    real(dp) :: phev_petrol = -2.635_dp
    real(dp) :: e85 = -0.432_dp
    real(dp) :: gas = -3.946_dp
    real(dp) :: hev_diesel = -1.144_dp
    real(dp) :: hev_petrol = -0.515_dp
    real(dp) :: bev = -2.158_dp
    real(dp) :: diesel = -1.087_dp
    real(dp) :: many_cars_hh = 0.272_dp
    real(dp) :: detached_house = 0.614_dp
    real(dp) :: grant = 0.477_dp
    real(dp) :: public_charging_home = 0.022_dp
    real(dp) :: public_charging_work = 0.011_dp
    real(dp) :: rural_area = 0.0_dp
    real(dp) :: small_city = -0.38_dp
    real(dp) :: make_constant(N_MAKES) = [ &
      3.205_dp, 3.345_dp, 3.050_dp, 2.712_dp, 2.220_dp, 2.753_dp, 2.365_dp, -7.305_dp, &
      2.073_dp, 1.578_dp, 1.783_dp, 1.718_dp, 4.517_dp, 1.646_dp, 1.016_dp, 1.721_dp, &
      1.209_dp, 3.790_dp, 2.611_dp, 0.513_dp, 1.830_dp, 0.729_dp, -0.055_dp, 1.404_dp, &
      -0.368_dp, 3.873_dp, 2.305_dp, 3.952_dp, 4.379_dp, 3.000_dp, -0.721_dp, 0.000_dp ]
  end type ModelParameters

  type :: CarPrepared
    integer :: choice_ut = 0
    integer :: lopnr = 0
    integer :: make = 0
    integer :: fueltype = 0
    real(dp) :: adjusted_price_ksek = 0.0_dp
    real(dp) :: adjusted_fossil_consumption = 0.0_dp
    real(dp) :: adjusted_electric_consumption = 0.0_dp
    real(dp) :: adjusted_range = 0.0_dp
    real(dp) :: adjusted_co2 = 0.0_dp
    real(dp) :: op_cost_ksek = 0.0_dp
    real(dp) :: utility_no_price = 0.0_dp
    logical :: supports_electric_zone_terms = .false.
  end type CarPrepared
end module microsim_types

module microsim_utils
  use microsim_kinds
  implicit none
contains
  subroutine normalize_delimiters(line)
    character(len=*), intent(inout) :: line
    integer :: i
    do i = 1, len(line)
      select case (line(i:i))
      case (',', ';', achar(9))
        line(i:i) = ' '
      end select
    end do
  end subroutine normalize_delimiters

  logical function is_skip_line(line)
    character(len=*), intent(in) :: line
    character(len=:), allocatable :: trimmed
    trimmed = adjustl(line)
    if (len_trim(trimmed) == 0) then
      is_skip_line = .true.
    else
      is_skip_line = trimmed(1:1) == '#' .or. trimmed(1:1) == '!'
    end if
  end function is_skip_line

  pure real(dp) function safe_log(x)
    real(dp), intent(in) :: x
    safe_log = log(max(x, 1.0e-9_dp))
  end function safe_log

  integer function income_class_from_value(income_ksek, lower, upper)
    real(dp), intent(in) :: income_ksek
    real(dp), intent(in) :: lower(N_INCOME_CLASSES), upper(N_INCOME_CLASSES)
    integer :: i
    income_class_from_value = N_INCOME_CLASSES
    do i = 1, N_INCOME_CLASSES
      if (upper(i) < 0.0_dp) then
        if (income_ksek >= lower(i)) then
          income_class_from_value = i
          return
        end if
      else
        if (income_ksek >= lower(i) .and. income_ksek <= upper(i)) then
          income_class_from_value = i
          return
        end if
      end if
    end do
  end function income_class_from_value

  integer function output_income_class(income_class)
    integer, intent(in) :: income_class
    output_income_class = min(max(income_class, 1), N_OUTPUT_INCOME)
  end function output_income_class

  subroutine softmax(utilities, probs, n)
    integer, intent(in) :: n
    real(dp), intent(in) :: utilities(n)
    real(dp), intent(out) :: probs(n)
    real(dp) :: max_u, denominator
    integer :: i
    max_u = maxval(utilities(1:n))
    denominator = 0.0_dp
    do i = 1, n
      probs(i) = exp(min(700.0_dp, utilities(i) - max_u))
      denominator = denominator + probs(i)
    end do
    if (denominator <= 0.0_dp) then
      probs(1:n) = 1.0_dp / real(n, dp)
    else
      probs(1:n) = probs(1:n) / denominator
    end if
  end subroutine softmax

  pure real(dp) function yearly_tax_sek(fueltype, co2, tax_multiplier)
    integer, intent(in) :: fueltype
    real(dp), intent(in) :: co2, tax_multiplier
    real(dp) :: base

    base = 0.0_dp
    if (fueltype == 1 .or. fueltype == 5) then
      base = 360.0_dp + 107.0_dp * min(max(co2 - 75.0_dp, 0.0_dp), 50.0_dp) + 132.0_dp * max(co2 - 125.0_dp, 0.0_dp)
    else if (fueltype == 2 .or. fueltype == 6) then
      base = 360.0_dp + 107.0_dp * min(max(co2 - 75.0_dp, 0.0_dp), 50.0_dp) + 132.0_dp * max(co2 - 125.0_dp, 0.0_dp) + &
        co2 * 13.52_dp + 250.0_dp
    end if
    yearly_tax_sek = base * tax_multiplier
  end function yearly_tax_sek

  integer function hash_lookup(key, keys, vals, empty_key)
    integer(int64), intent(in) :: key
    integer(int64), intent(in) :: keys(:)
    integer, intent(in) :: vals(:)
    integer(int64), intent(in) :: empty_key
    integer :: slot, probe, table_size

    table_size = size(keys)
    slot = int(modulo(key, int(table_size, int64))) + 1
    hash_lookup = 0
    do probe = 1, table_size
      if (keys(slot) == empty_key) return
      if (keys(slot) == key) then
        hash_lookup = vals(slot)
        return
      end if
      slot = slot + 1
      if (slot > table_size) slot = 1
    end do
  end function hash_lookup

  pure function to_lower(text) result(lower_text)
    character(len=*), intent(in) :: text
    character(len=len(text)) :: lower_text
    integer :: i, code
    do i = 1, len(text)
      code = iachar(text(i:i))
      if (code >= iachar('A') .and. code <= iachar('Z')) then
        lower_text(i:i) = achar(code + 32)
      else
        lower_text(i:i) = text(i:i)
      end if
    end do
  end function to_lower

  logical function token_to_real(token, value)
    character(len=*), intent(in) :: token
    real(dp), intent(out) :: value
    integer :: ios
    read(token, *, iostat=ios) value
    token_to_real = (ios == 0)
  end function token_to_real
end module microsim_utils

module microsim_io
  use microsim_kinds
  use microsim_types
  use microsim_utils
  implicit none
contains
  subroutine read_population(path, households, n_households)
    character(len=*), intent(in) :: path
    type(Household), intent(out) :: households(MAX_HOUSEHOLDS)
    integer, intent(out) :: n_households

    integer :: unit, ios, row_count
    character(len=4096) :: line
    integer :: zone_number, record_number, person_id
    integer :: hh_bost, hh_n_arb, hh_n_bil, hh_n_kk, hh_typ, kort
    integer :: p0_age, p0_forv, p0_kk, p0_kort, p0_sex, sample_hh_id
    integer(int64) :: household_id
    real(dp) :: hh_ink, p0_ink, weight, ant_vux, ant_barn, hhvikt
    integer(int64), allocatable :: hash_keys(:)
    integer, allocatable :: hash_vals(:)
    integer :: hash_size
    logical :: is_new
    integer :: idx

    hash_size = 1048583
    allocate(hash_keys(hash_size), hash_vals(hash_size))
    hash_keys = EMPTY_KEY
    hash_vals = 0

    n_households = 0
    row_count = 0

    open(newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
    if (ios /= 0) then
      write(*, '(A)') 'Could not open population file: '//trim(path)
      stop 1
    end if

    do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (is_skip_line(line)) cycle
      call normalize_delimiters(line)
      read(line, *, iostat=ios) zone_number, record_number, household_id, person_id, &
        hh_bost, hh_ink, hh_n_arb, hh_n_bil, hh_n_kk, hh_typ, kort, p0_age, p0_forv, &
        p0_ink, p0_kk, p0_kort, p0_sex, sample_hh_id, weight, ant_vux, ant_barn, hhvikt
      if (ios /= 0) cycle

      row_count = row_count + 1
      call household_hash_insert_or_get(household_id, hash_keys, hash_vals, idx, is_new)
      if (is_new) then
        n_households = n_households + 1
        if (n_households > MAX_HOUSEHOLDS) then
          write(*, '(A, I0)') 'MAX_HOUSEHOLDS exceeded: ', MAX_HOUSEHOLDS
          stop 1
        end if
        households(n_households)%zone_number = zone_number
        households(n_households)%household_id = household_id
        households(n_households)%hh_bost = hh_bost
        households(n_households)%hh_ink = hh_ink
        households(n_households)%hh_n_bil = hh_n_bil
        households(n_households)%hh_n_kk = hh_n_kk
        households(n_households)%ant_vux = ant_vux
        households(n_households)%ant_barn = ant_barn
        households(n_households)%hhvikt = max(hhvikt, 0.0_dp)
      end if
    end do
    close(unit)

    write(*, '(A, I0, A, I0)') 'Population rows read: ', row_count, ', unique households: ', n_households

    deallocate(hash_keys, hash_vals)
  contains
    subroutine household_hash_insert_or_get(key, keys, vals, value_index, is_new)
      integer(int64), intent(in) :: key
      integer(int64), intent(inout) :: keys(:)
      integer, intent(inout) :: vals(:)
      integer, intent(out) :: value_index
      logical, intent(out) :: is_new
      integer :: slot, probe, table_size

      table_size = size(keys)
      slot = int(modulo(key, int(table_size, int64))) + 1
      do probe = 1, table_size
        if (keys(slot) == EMPTY_KEY) then
          keys(slot) = key
          vals(slot) = n_households + 1
          value_index = vals(slot)
          is_new = .true.
          return
        else if (keys(slot) == key) then
          value_index = vals(slot)
          is_new = .false.
          return
        end if
        slot = slot + 1
        if (slot > table_size) slot = 1
      end do
      write(*, '(A)') 'Household hash table full.'
      stop 1
    end subroutine household_hash_insert_or_get
  end subroutine read_population

  subroutine read_zone_data(path, zones, n_zones, zone_hash_keys, zone_hash_vals)
    character(len=*), intent(in) :: path
    type(ZoneDatum), intent(out) :: zones(MAX_ZONES)
    integer, intent(out) :: n_zones
    integer(int64), allocatable, intent(out) :: zone_hash_keys(:)
    integer, allocatable, intent(out) :: zone_hash_vals(:)

    integer :: unit, ios
    character(len=4096) :: line
    integer :: zone_number, idx
    real(dp) :: detached_house, grant, public_home, public_work, rural_area, small_city
    integer :: hash_size
    logical :: is_new

    hash_size = 65537
    allocate(zone_hash_keys(hash_size), zone_hash_vals(hash_size))
    zone_hash_keys = EMPTY_KEY
    zone_hash_vals = 0
    n_zones = 0

    open(newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
    if (ios /= 0) then
      write(*, '(A)') 'Could not open zone file: '//trim(path)
      stop 1
    end if

    do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (is_skip_line(line)) cycle
      call normalize_delimiters(line)
      read(line, *, iostat=ios) zone_number, detached_house, grant, public_home, public_work, rural_area, small_city
      if (ios /= 0) cycle

      call zone_hash_insert_or_get(int(zone_number, int64), zone_hash_keys, zone_hash_vals, idx, is_new)
      if (is_new) then
        n_zones = n_zones + 1
        if (n_zones > MAX_ZONES) then
          write(*, '(A, I0)') 'MAX_ZONES exceeded: ', MAX_ZONES
          stop 1
        end if
      end if
      zones(idx)%zone_number = zone_number
      zones(idx)%detached_house = detached_house
      zones(idx)%grant = grant
      zones(idx)%public_charging_home = public_home
      zones(idx)%public_charging_work = public_work
      zones(idx)%rural_area = rural_area
      zones(idx)%small_city = small_city
    end do
    close(unit)

    write(*, '(A, I0)') 'Zones read: ', n_zones

  contains
    subroutine zone_hash_insert_or_get(key, keys, vals, value_index, is_new)
      integer(int64), intent(in) :: key
      integer(int64), intent(inout) :: keys(:)
      integer, intent(inout) :: vals(:)
      integer, intent(out) :: value_index
      logical, intent(out) :: is_new
      integer :: slot, probe, table_size

      table_size = size(keys)
      slot = int(modulo(key, int(table_size, int64))) + 1
      do probe = 1, table_size
        if (keys(slot) == EMPTY_KEY) then
          keys(slot) = key
          vals(slot) = n_zones + 1
          value_index = vals(slot)
          is_new = .true.
          return
        else if (keys(slot) == key) then
          value_index = vals(slot)
          is_new = .false.
          return
        end if
        slot = slot + 1
        if (slot > table_size) slot = 1
      end do
      write(*, '(A)') 'Zone hash table full.'
      stop 1
    end subroutine zone_hash_insert_or_get
  end subroutine read_zone_data

  subroutine read_car_alternatives(path, cars, n_cars)
    character(len=*), intent(in) :: path
    type(CarAlternative), intent(out) :: cars(MAX_CARS)
    integer, intent(out) :: n_cars
    integer :: unit, ios
    character(len=4096) :: line

    n_cars = 0

    open(newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
    if (ios /= 0) then
      write(*, '(A)') 'Could not open car alternatives file: '//trim(path)
      stop 1
    end if

    do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (is_skip_line(line)) cycle
      call normalize_delimiters(line)

      n_cars = n_cars + 1
      if (n_cars > MAX_CARS) then
        write(*, '(A, I0)') 'MAX_CARS exceeded: ', MAX_CARS
        stop 1
      end if

      read(line, *, iostat=ios) cars(n_cars)%choice_ut, cars(n_cars)%lopnr, cars(n_cars)%altnr, &
        cars(n_cars)%make, cars(n_cars)%fueltype, cars(n_cars)%fclass, cars(n_cars)%kombi, &
        cars(n_cars)%rust, cars(n_cars)%curbw, cars(n_cars)%lprice, cars(n_cars)%nco9, &
        cars(n_cars)%ncc09, cars(n_cars)%rangee, cars(n_cars)%fuelcons, cars(n_cars)%fuelcons_el, cars(n_cars)%segut, &
        cars(n_cars)%storlekut, cars(n_cars)%karossut, cars(n_cars)%co2
      if (ios /= 0) then
        n_cars = n_cars - 1
      end if
    end do
    close(unit)

    write(*, '(A, I0)') 'Car alternatives read: ', n_cars
  end subroutine read_car_alternatives

  subroutine read_fuel_prices(path, fuel_prices)
    character(len=*), intent(in) :: path
    real(dp), intent(out) :: fuel_prices(N_FUEL_TYPES)
    integer :: unit, ios, fuel_type
    real(dp) :: price
    character(len=4096) :: line

    fuel_prices = 0.0_dp
    open(newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
    if (ios /= 0) then
      write(*, '(A)') 'Could not open fuel price file: '//trim(path)
      stop 1
    end if

    do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (is_skip_line(line)) cycle
      call normalize_delimiters(line)
      read(line, *, iostat=ios) fuel_type, price
      if (ios /= 0) cycle
      if (fuel_type >= 1 .and. fuel_type <= N_FUEL_TYPES) fuel_prices(fuel_type) = price
    end do
    close(unit)

    write(*, '(A)') 'Fuel prices loaded.'
  end subroutine read_fuel_prices

  subroutine read_income_classes(path, lower, upper)
    character(len=*), intent(in) :: path
    real(dp), intent(out) :: lower(N_INCOME_CLASSES), upper(N_INCOME_CLASSES)
    logical :: exists
    integer :: unit, ios, n_read
    character(len=4096) :: line
    character(len=64) :: t1, t2, t3
    real(dp) :: low_value, up_value
    logical :: ok1, ok2

    lower = [0.0_dp, 401.0_dp, 801.0_dp, 1601.0_dp, 2401.0_dp]
    upper = [400.0_dp, 800.0_dp, 1600.0_dp, 2400.0_dp, -1.0_dp]

    inquire(file=trim(path), exist=exists)
    if (.not. exists) then
      write(*, '(A)') 'Income class file not found, using defaults.'
      return
    end if

    open(newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
    if (ios /= 0) return

    n_read = 0
    do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (is_skip_line(line)) cycle
      call normalize_delimiters(line)

      t1 = ''
      t2 = ''
      t3 = ''
      read(line, *, iostat=ios) t1, t2, t3
      if (ios /= 0) cycle

      ok1 = token_to_real(t1, low_value)
      ok2 = token_to_real(t2, up_value)
      if (ok1 .and. ok2) then
        n_read = n_read + 1
        if (n_read <= N_INCOME_CLASSES) then
          lower(n_read) = low_value
          upper(n_read) = up_value
        end if
      else
        ok1 = token_to_real(t2, low_value)
        if (.not. ok1) cycle
        if (trim(t3) == '-' .or. trim(to_lower(t3)) == 'inf') then
          up_value = -1.0_dp
        else
          ok2 = token_to_real(t3, up_value)
          if (.not. ok2) cycle
        end if
        n_read = n_read + 1
        if (n_read <= N_INCOME_CLASSES) then
          lower(n_read) = low_value
          upper(n_read) = up_value
        end if
      end if
      if (n_read >= N_INCOME_CLASSES) exit
    end do
    close(unit)

    write(*, '(A)') 'Income class definition loaded.'
  end subroutine read_income_classes

  subroutine read_scenario(path, scenario)
    character(len=*), intent(in) :: path
    type(ScenarioSettings), intent(inout) :: scenario
    logical :: exists
    integer :: unit, ios
    character(len=4096) :: line
    character(len=128) :: key
    real(dp) :: value
    character(len=128) :: lower_key

    if (len_trim(path) == 0) return
    if (trim(to_lower(path)) == 'none') return

    inquire(file=trim(path), exist=exists)
    if (.not. exists) then
      write(*, '(A)') 'Scenario file not found, using defaults.'
      return
    end if

    open(newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
    if (ios /= 0) return

    do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (is_skip_line(line)) cycle
      call normalize_delimiters(line)
      read(line, *, iostat=ios) key, value
      if (ios /= 0) cycle

      lower_key = trim(to_lower(key))
      select case (trim(lower_key))
      case ('fossil_consumption_factor')
        scenario%fossil_consumption_factor = value
      case ('electric_consumption_factor')
        scenario%electric_consumption_factor = value
      case ('ev_range_factor')
        scenario%ev_range_factor = value
      case ('purchase_price_factor')
        scenario%purchase_price_factor = value
      case ('purchase_price_add_ksek')
        scenario%purchase_price_add_ksek = value
      case ('co2_factor')
        scenario%co2_factor = value
      case ('tax_multiplier')
        scenario%tax_multiplier = value
      case ('grant_multiplier')
        scenario%grant_multiplier = value
      end select
    end do
    close(unit)

    write(*, '(A)') 'Scenario settings loaded.'
  end subroutine read_scenario

  subroutine write_adjusted_alternatives(output_dir, cars, prepared, n_cars)
    character(len=*), intent(in) :: output_dir
    type(CarAlternative), intent(in) :: cars(MAX_CARS)
    type(CarPrepared), intent(in) :: prepared(:)
    integer, intent(in) :: n_cars
    character(len=1024) :: path
    integer :: unit, ios, i

    path = build_output_path(output_dir, 'adjusted_car_alternatives.csv')
    open(newunit=unit, file=trim(path), status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*, '(A)') 'Could not write file: '//trim(path)
      stop 1
    end if

    write(unit, '(A)') 'Choice_ut,Lopnr,Make,Fueltype,AdjustedPrice_kSEK,'// &
      'AdjustedFossilCons,AdjustedElectricCons,AdjustedRange,AdjustedCO2,OpCost_kSEK'
    do i = 1, n_cars
      write(unit, '(I0, ",", I0, ",", I0, ",", I0, ",", F12.5, ",", F12.5, ",", F12.5, ",", F12.5, ",", F12.5, ",", F12.5)') &
        cars(i)%choice_ut, cars(i)%lopnr, cars(i)%make, cars(i)%fueltype, prepared(i)%adjusted_price_ksek, &
        prepared(i)%adjusted_fossil_consumption, prepared(i)%adjusted_electric_consumption, prepared(i)%adjusted_range, &
        prepared(i)%adjusted_co2, prepared(i)%op_cost_ksek
    end do
    close(unit)
  end subroutine write_adjusted_alternatives

  subroutine write_alternative_results(output_dir, cars, expected_alt, n_cars)
    character(len=*), intent(in) :: output_dir
    type(CarAlternative), intent(in) :: cars(MAX_CARS)
    real(dp), intent(in) :: expected_alt(:)
    integer, intent(in) :: n_cars
    character(len=1024) :: path
    integer :: unit, ios, i

    path = build_output_path(output_dir, 'expected_by_alternative.csv')
    open(newunit=unit, file=trim(path), status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*, '(A)') 'Could not write file: '//trim(path)
      stop 1
    end if

    write(unit, '(A)') 'Choice_ut,Lopnr,Make,Fueltype,ExpectedDemand'
    do i = 1, n_cars
      write(unit, '(I0, ",", I0, ",", I0, ",", I0, ",", F16.6)') &
        cars(i)%choice_ut, cars(i)%lopnr, cars(i)%make, cars(i)%fueltype, expected_alt(i)
    end do
    close(unit)
  end subroutine write_alternative_results

  subroutine write_national_results(output_dir, national)
    character(len=*), intent(in) :: output_dir
    real(dp), intent(in) :: national(N_FUEL_TYPES, N_OUTPUT_INCOME)
    character(len=1024) :: path
    character(len=8), parameter :: income_labels(N_OUTPUT_INCOME) = [ 'HI1     ', 'HI2     ', 'HI3     ', 'HI4plus ' ]
    integer :: unit, ios, fuel_type, inc

    path = build_output_path(output_dir, 'national_fuel_income.csv')
    open(newunit=unit, file=trim(path), status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*, '(A)') 'Could not write file: '//trim(path)
      stop 1
    end if

    write(unit, '(A)') 'FuelType,IncomeClass,ExpectedDemand'
    do fuel_type = 1, N_FUEL_TYPES
      do inc = 1, N_OUTPUT_INCOME
        write(unit, '(I0, ",", A, ",", F16.6)') fuel_type, trim(income_labels(inc)), national(fuel_type, inc)
      end do
    end do
    close(unit)
  end subroutine write_national_results

  subroutine write_zonal_results(output_dir, zones, zonal, n_zones)
    character(len=*), intent(in) :: output_dir
    type(ZoneDatum), intent(in) :: zones(MAX_ZONES)
    real(dp), intent(in) :: zonal(MAX_ZONES, N_FUEL_TYPES, N_OUTPUT_INCOME)
    integer, intent(in) :: n_zones
    character(len=1024) :: path
    character(len=8), parameter :: income_labels(N_OUTPUT_INCOME) = [ 'HI1     ', 'HI2     ', 'HI3     ', 'HI4plus ' ]
    integer :: unit, ios, zone_idx, fuel_type, inc

    path = build_output_path(output_dir, 'zonal_fuel_income.csv')
    open(newunit=unit, file=trim(path), status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*, '(A)') 'Could not write file: '//trim(path)
      stop 1
    end if

    write(unit, '(A)') 'ZoneNumber,FuelType,IncomeClass,ExpectedDemand'
    do zone_idx = 1, n_zones
      do fuel_type = 1, N_FUEL_TYPES
        do inc = 1, N_OUTPUT_INCOME
          write(unit, '(I0, ",", I0, ",", A, ",", F16.6)') &
            zones(zone_idx)%zone_number, fuel_type, trim(income_labels(inc)), &
            zonal(zone_idx, fuel_type, inc)
        end do
      end do
    end do
    close(unit)
  end subroutine write_zonal_results

  function build_output_path(output_dir, filename) result(path)
    character(len=*), intent(in) :: output_dir, filename
    character(len=1024) :: path
    integer :: last_char

    if (len_trim(output_dir) == 0 .or. trim(output_dir) == '.') then
      path = trim(filename)
      return
    end if

    last_char = len_trim(output_dir)
    if (output_dir(last_char:last_char) == '/') then
      path = trim(output_dir)//trim(filename)
    else
      path = trim(output_dir)//'/'//trim(filename)
    end if
  end function build_output_path
end module microsim_io

module microsim_model
  use microsim_kinds
  use microsim_types
  use microsim_utils
  implicit none
contains
  subroutine prepare_alternatives(cars, n_cars, params, scenario, fuel_prices, prepared)
    type(CarAlternative), intent(in) :: cars(MAX_CARS)
    integer, intent(in) :: n_cars
    type(ModelParameters), intent(in) :: params
    type(ScenarioSettings), intent(in) :: scenario
    real(dp), intent(in) :: fuel_prices(N_FUEL_TYPES)
    type(CarPrepared), intent(out) :: prepared(n_cars)
    integer :: i
    real(dp) :: fossil_cons, electric_cons, range_adj, price_adj, co2_adj
    real(dp) :: op_cost_ksek

    do i = 1, n_cars
      fossil_cons = cars(i)%fuelcons_el
      electric_cons = cars(i)%fuelcons
      range_adj = cars(i)%rangee
      price_adj = cars(i)%lprice * scenario%purchase_price_factor + scenario%purchase_price_add_ksek
      co2_adj = cars(i)%co2 * scenario%co2_factor

      if (cars(i)%fueltype /= 3) fossil_cons = fossil_cons * scenario%fossil_consumption_factor
      electric_cons = electric_cons * scenario%electric_consumption_factor
      if (cars(i)%fueltype == 3 .or. cars(i)%fueltype == 9 .or. cars(i)%fueltype == 10) then
        range_adj = range_adj * scenario%ev_range_factor
      end if

      if (cars(i)%fueltype == 3) then
        op_cost_ksek = (electric_cons * 150.0_dp * fuel_prices(cars(i)%fueltype) + &
          yearly_tax_sek(cars(i)%fueltype, co2_adj, scenario%tax_multiplier)) / 1000.0_dp
      else
        op_cost_ksek = (fossil_cons * 150.0_dp * fuel_prices(cars(i)%fueltype) + &
          yearly_tax_sek(cars(i)%fueltype, co2_adj, scenario%tax_multiplier)) / 1000.0_dp
      end if

      prepared(i)%choice_ut = cars(i)%choice_ut
      prepared(i)%lopnr = cars(i)%lopnr
      prepared(i)%make = cars(i)%make
      prepared(i)%fueltype = cars(i)%fueltype
      prepared(i)%adjusted_price_ksek = max(price_adj, 0.0_dp)
      prepared(i)%adjusted_fossil_consumption = max(fossil_cons, 0.0_dp)
      prepared(i)%adjusted_electric_consumption = max(electric_cons, 0.0_dp)
      prepared(i)%adjusted_range = max(range_adj, 0.0_dp)
      prepared(i)%adjusted_co2 = max(co2_adj, 0.0_dp)
      prepared(i)%op_cost_ksek = max(op_cost_ksek, 0.0_dp)
      prepared(i)%supports_electric_zone_terms = (cars(i)%fueltype == 3 .or. &
        cars(i)%fueltype == 9 .or. cars(i)%fueltype == 10)
      prepared(i)%utility_no_price = static_utility_component(cars(i), prepared(i)%op_cost_ksek, params)
    end do
  end subroutine prepare_alternatives

  real(dp) function static_utility_component(car, op_cost_ksek, params)
    type(CarAlternative), intent(in) :: car
    real(dp), intent(in) :: op_cost_ksek
    type(ModelParameters), intent(in) :: params
    real(dp) :: size_mid, size_large, size_sport
    real(dp) :: fuel_term

    size_mid = 0.0_dp
    size_large = 0.0_dp
    size_sport = 0.0_dp
    if (car%storlekut == 2 .or. car%storlekut == 3) size_mid = 1.0_dp
    if (car%storlekut == 4 .or. car%storlekut == 5) size_large = 1.0_dp
    if (car%storlekut == 6 .or. car%storlekut == 7) size_sport = 1.0_dp

    fuel_term = 0.0_dp
    select case (car%fueltype)
    case (10)
      fuel_term = params%phev_diesel
    case (9)
      fuel_term = params%phev_petrol
    case (8)
      fuel_term = params%e85
    case (7)
      fuel_term = params%gas
    case (6)
      fuel_term = params%hev_diesel
    case (5)
      fuel_term = params%hev_petrol
    case (3)
      fuel_term = params%bev
    case (2)
      fuel_term = params%diesel
    case default
      fuel_term = 0.0_dp
    end select

    static_utility_component = params%op_cost * op_cost_ksek + params%rust_guarantee * car%rust + &
      params%passenger_safety * car%nco9 + params%safety_systems * car%ncc09 + &
      params%size_mid * size_mid + params%size_large * size_large + &
      params%size_sport * size_sport + fuel_term

    if (car%make >= 1 .and. car%make <= N_MAKES) then
      static_utility_component = static_utility_component + params%make_constant(car%make)
    end if
  end function static_utility_component

  subroutine run_simulation(households, n_households, zones, zone_hash_keys, zone_hash_vals, prepared, n_cars, &
      income_lower, income_upper, params, scenario, expected_alt, national, zonal)
    type(Household), intent(in) :: households(MAX_HOUSEHOLDS)
    integer, intent(in) :: n_households
    type(ZoneDatum), intent(in) :: zones(MAX_ZONES)
    integer(int64), intent(in) :: zone_hash_keys(:)
    integer, intent(in) :: zone_hash_vals(:)
    type(CarPrepared), intent(in) :: prepared(:)
    integer, intent(in) :: n_cars
    real(dp), intent(in) :: income_lower(N_INCOME_CLASSES), income_upper(N_INCOME_CLASSES)
    type(ModelParameters), intent(in) :: params
    type(ScenarioSettings), intent(in) :: scenario
    real(dp), intent(out) :: expected_alt(n_cars)
    real(dp), intent(out) :: national(N_FUEL_TYPES, N_OUTPUT_INCOME)
    real(dp), intent(out) :: zonal(MAX_ZONES, N_FUEL_TYPES, N_OUTPUT_INCOME)

    real(dp), allocatable :: utilities(:), probs(:)
    integer :: hh_idx, car_idx, income_class, output_class, zone_idx, fuel_type
    real(dp) :: income_ksek, p_newcar, contribution
    type(ZoneDatum) :: zone

    allocate(utilities(n_cars), probs(n_cars))
    expected_alt = 0.0_dp
    national = 0.0_dp
    zonal = 0.0_dp

    do hh_idx = 1, n_households
      income_ksek = households(hh_idx)%hh_ink / 1000.0_dp
      income_class = income_class_from_value(income_ksek, income_lower, income_upper)
      output_class = output_income_class(income_class)
      p_newcar = new_car_probability(households(hh_idx), params)

      zone_idx = hash_lookup(int(households(hh_idx)%zone_number, int64), zone_hash_keys, zone_hash_vals, EMPTY_KEY)
      if (zone_idx > 0) then
        zone = zones(zone_idx)
      else
        zone = ZoneDatum()
      end if

      do car_idx = 1, n_cars
        utilities(car_idx) = prepared(car_idx)%utility_no_price + &
          params%price_hi(income_class) * prepared(car_idx)%adjusted_price_ksek
        if (prepared(car_idx)%supports_electric_zone_terms) then
          if (households(hh_idx)%hh_n_bil > 1) utilities(car_idx) = utilities(car_idx) + params%many_cars_hh
          utilities(car_idx) = utilities(car_idx) + params%detached_house * zone%detached_house + &
            params%grant * (zone%grant * scenario%grant_multiplier) + params%public_charging_home * zone%public_charging_home + &
            params%public_charging_work * zone%public_charging_work + params%rural_area * zone%rural_area + &
            params%small_city * zone%small_city
        end if
      end do

      call softmax(utilities, probs, n_cars)

      do car_idx = 1, n_cars
        contribution = households(hh_idx)%hhvikt * p_newcar * probs(car_idx)
        expected_alt(car_idx) = expected_alt(car_idx) + contribution
        fuel_type = prepared(car_idx)%fueltype
        if (fuel_type >= 1 .and. fuel_type <= N_FUEL_TYPES) then
          national(fuel_type, output_class) = national(fuel_type, output_class) + contribution
          if (zone_idx > 0) zonal(zone_idx, fuel_type, output_class) = zonal(zone_idx, fuel_type, output_class) + contribution
        end if
      end do
    end do

    deallocate(utilities, probs)
  end subroutine run_simulation

  real(dp) function new_car_probability(hh, params)
    type(Household), intent(in) :: hh
    type(ModelParameters), intent(in) :: params
    real(dp) :: income_ksek, utility_buy, utility_not_buy, diff

    income_ksek = max(hh%hh_ink / 1000.0_dp, 1.0e-9_dp)
    utility_buy = params%buy_ln_ink * safe_log(income_ksek) + params%buy_antpers * (hh%ant_vux + hh%ant_barn) + &
      params%buy_inkomst * income_ksek + params%buy_hustyp * real(hh%hh_bost, dp) + params%buy_kk_ant * real(hh%hh_n_kk, dp)
    utility_not_buy = params%no_buy_constant

    diff = utility_not_buy - utility_buy
    if (diff > 700.0_dp) then
      new_car_probability = 0.0_dp
    else if (diff < -700.0_dp) then
      new_car_probability = 1.0_dp
    else
      new_car_probability = 1.0_dp / (1.0_dp + exp(diff))
    end if
  end function new_car_probability
end module microsim_model

program microsim_main
  use microsim_kinds
  use microsim_types
  use microsim_io
  use microsim_model
  use, intrinsic :: iso_fortran_env, only: int64
  implicit none

  character(len=1024), parameter :: population_file = 'testdata/population.dat'
  character(len=1024), parameter :: zone_file = 'testdata/zones.dat'
  character(len=1024), parameter :: car_file = 'testdata/cars.dat'
  character(len=1024), parameter :: fuel_file = 'testdata/fuelprice.dat'
  character(len=1024), parameter :: income_file = 'testdata/hhclass.dat'
  character(len=1024), parameter :: output_dir = 'output'
  character(len=1024), parameter :: scenario_file = 'testdata/scenario.dat'
  integer(int64), parameter :: MEMORY_LIMIT_4GIB = 4_int64 * 1024_int64 * 1024_int64 * 1024_int64

  type(Household) :: households(MAX_HOUSEHOLDS)
  type(ZoneDatum) :: zones(MAX_ZONES)
  type(CarAlternative) :: cars(MAX_CARS)
  type(CarPrepared), allocatable :: prepared(:)
  type(ModelParameters) :: params
  type(ScenarioSettings) :: scenario

  integer :: n_households, n_zones, n_cars
  integer(int64), allocatable :: zone_hash_keys(:)
  integer, allocatable :: zone_hash_vals(:)
  real(dp) :: fuel_prices(N_FUEL_TYPES)
  real(dp) :: income_lower(N_INCOME_CLASSES), income_upper(N_INCOME_CLASSES)
  real(dp), allocatable :: expected_alt(:)
  real(dp) :: national(N_FUEL_TYPES, N_OUTPUT_INCOME)
  real(dp) :: zonal(MAX_ZONES, N_FUEL_TYPES, N_OUTPUT_INCOME)
  integer(int64) :: size_households, size_zones, size_cars
  integer(int64) :: size_national, size_zonal, size_misc
  integer(int64) :: total_static_bytes

  size_households = int(storage_size(households) / 8, int64)
  size_zones = int(storage_size(zones) / 8, int64)
  size_cars = int(storage_size(cars) / 8, int64)
  size_national = int(storage_size(national) / 8, int64)
  size_zonal = int(storage_size(zonal) / 8, int64)
  size_misc = int(1048583_int64 * 8_int64 + 1048583_int64 * 4_int64, int64)
  total_static_bytes = size_households + size_zones + size_cars + size_national + size_zonal + size_misc

  write(*, '(A, I0, A, F8.3, A)') 'Static memory estimate: ', total_static_bytes, ' bytes (', &
    real(total_static_bytes, dp) / (1024.0_dp * 1024.0_dp * 1024.0_dp), ' GiB)'
  if (total_static_bytes > MEMORY_LIMIT_4GIB) then
    write(*, '(A)') 'Configuration exceeds 4 GiB static memory budget.'
    stop 1
  end if

  call read_population(population_file, households, n_households)
  call read_zone_data(zone_file, zones, n_zones, zone_hash_keys, zone_hash_vals)
  call read_car_alternatives(car_file, cars, n_cars)
  call read_fuel_prices(fuel_file, fuel_prices)
  call read_income_classes(income_file, income_lower, income_upper)
  call read_scenario(scenario_file, scenario)

  allocate(prepared(n_cars), expected_alt(n_cars))

  call prepare_alternatives(cars, n_cars, params, scenario, fuel_prices, prepared)
  call run_simulation(households, n_households, zones, zone_hash_keys, zone_hash_vals, &
    prepared, n_cars, income_lower, income_upper, params, scenario, expected_alt, national, zonal)

  call write_adjusted_alternatives(output_dir, cars, prepared, n_cars)
  call write_alternative_results(output_dir, cars, expected_alt, n_cars)
  call write_national_results(output_dir, national)
  call write_zonal_results(output_dir, zones, zonal, n_zones)

  write(*, '(A)') 'Microsimulation completed.'
  write(*, '(A, I0)') 'Households simulated: ', n_households
  write(*, '(A, I0)') 'Car alternatives simulated: ', n_cars
  write(*, '(A, I0)') 'Zones simulated: ', n_zones
end program microsim_main

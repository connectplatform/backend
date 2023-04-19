-module(im_geo_utils).

-include("im_common.hrl").

-export([
  calc_distance/2,
  filter_coor/1,
  filter_distance/1,
  filter_limit/1,
  check_range/3
]).

-define(EARTH_RADIUS, 6372795).
-define(DISTANCE,     200).
-define(MIN_DISTANCE, 1).
-define(MAX_DISTANCE, 1000000000).
-define(LIMIT, 50).
-define(MIN_LIMIT, 1).
-define(MAX_LIMIT, 1000000000).

calc_distance({Lat1, Lng1}, {Lat2, Lng2}) ->
  Pi = math:pi(),

  Lat1Rad = Lat1 * Pi / 180,
  Lng1Rad = Lng1 * Pi / 180,
  Lat2Rad = Lat2 * Pi / 180,
  Lng2Rad = Lng2 * Pi / 180,

  Cl1 = math:cos(Lat1Rad),
  Cl2 = math:cos(Lat2Rad),
  Sl1 = math:sin(Lat1Rad),
  Sl2 = math:sin(Lat2Rad),

  Delta = Lng2Rad - Lng1Rad,
  CDelta = math:cos(Delta),
  SDelta = math:sin(Delta),

  Y = math:sqrt(
    math:pow(Cl2 * SDelta, 2) +
      math:pow(Cl1*Sl2 - Sl1*Cl2 * CDelta, 2)
  ),
  X = Sl1 * Sl2 + Cl1*Cl2*CDelta,

  Ad = math:atan2(Y, X),
  Dist = Ad * ?EARTH_RADIUS,

  round(Dist).

filter_coor(Coor) when is_number(Coor) -> Coor;
filter_coor(Coor)                      -> format_float(Coor, 0.0).

filter_distance(Distance) when is_number(Distance) ->
  check_range(Distance, ?MIN_DISTANCE, ?MAX_DISTANCE);
filter_distance(RawDistance) ->
  Distance = format_float(RawDistance, ?DISTANCE),
  check_range(Distance, ?MIN_DISTANCE, ?MAX_DISTANCE).

filter_limit(Limit) when is_number(Limit) ->
  check_range(trunc(Limit), ?MIN_LIMIT, ?MAX_LIMIT);
filter_limit(RawLimit) ->
  Limit = format_float(RawLimit, ?LIMIT),
  check_range(trunc(Limit), ?MIN_LIMIT, ?MAX_LIMIT).

check_range(Value, _Min, Max) when Value > Max -> Max;
check_range(Value, Min, _Max) when Value < Min -> Min;
check_range(Value, _Min, _Max)                 -> Value.

format_float(Val, DefaultValue) ->
  try binary_to_list(Val) of
    L ->
      try list_to_float(L)
      catch error:badarg ->
        try float(list_to_integer(L))
        catch error:badarg -> DefaultValue end
      end
  catch error:badarg ->
    DefaultValue
  end.

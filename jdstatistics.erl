%% Statistics!
-module(jdstatistics).
-export([
		mean/1,
		meansquarederror/2, 
		squarederror/2]).

%% Calculates the mean of the supplied list
%% http://en.wikipedia.org/wiki/Arithmetic_mean
mean(List) ->
	lists:sum(List) / length(List).

%% Calculates the Mean Squared Error of the supplied lists
%% http://en.wikipedia.org/wiki/Mean_squared_error

%% Guard for different lengths
meansquarederror(Predictions, True) 
	when length(Predictions) =/= length(True) -> 
		erlang:error("Length of predictions must equal length of True values.");

%% Guard for zero-length lists
meansquarederror(Predictions, True)
	when (length(Predictions) =:= 0) or (length(True) =:= 0) ->
		erlang:error("Length of predictions or true values cannot be zero");

meansquarederror(Predictions, True) -> 
	1/length(Predictions) * meansquarederror_sumerrors(Predictions, True, 0).

%% Helper for meansquarederror -> sums the squarederror for both lists
meansquarederror_sumerrors(P, T, Sum) when (length(P) =:= 0) and (length(T) =:= 0) -> Sum;
meansquarederror_sumerrors(P, T, Sum) ->
	Sum + squarederror(hd(P), hd(T)) + meansquarederror_sumerrors(tl(P), tl(T), Sum).

%% Calculates the squared error between X and Y
squarederror(X,Y) -> math:pow((X-Y),2).
%% Statistics!
-module(jdstatistics).
-export([
		mean/1,
		mean_squared_error/2, 
		squared_error/2,
		pearson_correlation/2,
		entropy/2]).

%% Calculates the mean of the supplied list
%% http://en.wikipedia.org/wiki/Arithmetic_mean
mean(List) when length(List) =:= 0 -> erlang:error("List cannot be empty");
mean(List) ->
	lists:sum(List) / length(List).

%% Calculates the Mean Squared Error of the supplied lists
%% http://en.wikipedia.org/wiki/Mean_squared_error

%% Guard for different lengths
mean_squared_error(A, B) 
	when length(A) =/= length(B) -> 
		erlang:error("Length of A must equal length of B");

%% Guard for zero-length lists
mean_squared_error(A, B)
	when (length(A) =:= 0) or (length(B) =:= 0) ->
		erlang:error("Length of A or B cannot be zero");

mean_squared_error(A, B) -> 
	1/length(A) * lists:sum([squared_error(X,Y) || {X,Y} <- lists:zip(A,B)]).

%% Calculates the squared error between X and Y
squared_error(X,Y) -> math:pow((X-Y),2).


%% Calculates the Pearson correlation coefficient between A and B
%% http://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient

pearson_correlation(A,B) 
	when length(A) =/= length(B) ->
		erlang:error("Lengths of lists A and B must equal");

pearson_correlation(A,B)
	when (length(A) =:= 0) or (length(B) =:= 0) ->
		erlang:error("Length of A or B cannot be zero");

pearson_correlation(A,B) ->
	SumXY = lists:sum([X*Y || {X,Y} <- lists:zip(A,B)]),
	SumX = lists:sum(A),
	SumY = lists:sum(B),
	SumXX = lists:sum([L*L || L <- A]),
	SumYY = lists:sum([L*L || L <- B]),
	N = length(A),
	Numer = (N*SumXY) - (SumX * SumY),
	Denom = math:sqrt(((N*SumXX)-(SumX*SumX)) * ((N*SumYY)-(SumY*SumY))),
	Numer/Denom.

%% A logarithmic measure of the density of states
%% ProbabilityDistribution is a collection of probabilities of possible states.
%% http://en.wikipedia.org/wiki/Entropy

entropy(ProbabilityDistribution, Constant) when is_list(ProbabilityDistribution) ->
	-Constant * lists:sum([X*math:log(X) || X <- ProbabilityDistribution]).



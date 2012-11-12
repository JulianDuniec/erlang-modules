-module(jd3dvec).
-export([
	magnitude/1,
	add/2,
	subtract/2,
	multiply/2,
	distance/2,
	dot/2,
	normalize/1,
	rotateX/2,
	rotateY/2
]).

x(Vector) ->
	element(1, Vector).
y(Vector) ->
	element(2, Vector).
z(Vector) ->
	element(3, Vector).

magnitude (A) ->
	math:sqrt(
		  math:pow(x(A),2)
		+ math:pow(y(A),2)
		+ math:pow(z(A),3)).

add (A,B) ->
	{
		x(A)+x(B),
		y(A)+y(B),
		z(A)+z(B)
	}.

subtract (A,B) ->
	{
		x(A)-x(B),
		y(A)-y(B),
		z(A)-z(B)
	}.

multiply (A,B) ->
	{
		x(A)*x(B),
		y(A)*y(B),
		z(A)*z(B)
	}.

dot (A,B) ->
	  x(A) * x(B)
	+ y(A) * y(B)
	+ z(A) * z(B).

distance (A, B) ->
	magnitude(subtract(A,B)).


normalize (A) ->
	Length = magnitude(A),
	if 
		Length =:= 0 -> A;
		true ->
		{
			x(A) / Length,
			y(A) / Length,
			z(A) / Length
		}
	end.

rotateX (A, Angle) ->
	CosRY = math:cos(Angle),
	SinRY = math:sin(Angle),
	{
		x(A),
		(y(A) * CosRY) + (z(A) * SinRY),
		(y(A) * -SinRY) + (z(A) * CosRY)
	}.

rotateY (A, Angle) ->
	CosRY = math:cos(Angle),
	SinRY = math:sin(Angle),
	{
		(x(A) * CosRY) + (z(A) * SinRY),
		y(A),
		(x(A) * -SinRY) + (z(A) * CosRY)
	}.
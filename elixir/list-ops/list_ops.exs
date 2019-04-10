defmodule ListOps do

  @spec count(list) :: non_neg_integer
  def count(list), do: count(list, 0)
  def count([], n), do: n
  def count([_ | t], n), do: count(t, n + 1)

  @spec reverse(list) :: list
  def reverse([]), do: []

  def reverse(list, out \\ [])
  def reverse([], out), do: out
  def reverse([h | t], out), do: reverse(t, [h | out])

  @spec map(list, (any -> any)) :: list
  def map([], _), do: []
  def map([h | t], f), do: [f.(h) | map(t, f)]

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter([], _), do: []
  def filter([h | t], f) do
    if (f.(h)), do: [h | filter(t, f)], else: filter(t, f)
  end

  @type acc :: any
  @spec reduce(list, acc, (any, acc -> acc)) :: acc
  def reduce([], acc, _), do: acc
  def reduce([h | t], acc, f), do: reduce(t, f.(h, acc), f)

  @spec append(list, list) :: list
  def append([], []), do: []
  def append([], b), do: b
  def append(a, []), do: a
  def append([h | t], b), do: [h | append(t, b)]

  @spec concat([[any]]) :: [any]
  def concat([]), do: []
  def concat([a | t]), do: append(a, concat(t))
end

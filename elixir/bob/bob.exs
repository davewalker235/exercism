defmodule Bob do
  def hey(input) do
    input = cleanup(input)
    punctuation = String.last(input)
    case {yelling?(input), punctuation} do
      {true, "?"} -> "Calm down, I know what I'm doing!"
      {false, "?"} -> "Sure."
      {true, _} -> "Whoa, chill out!"
      {_, nil} -> "Fine. Be that way!"
      _ -> "Whatever."
    end
  end

  @doc """
  Trim whitespace and remove numbers. Numbers cause problems when determining
  if the string is all caps.
  """
  def cleanup(input) do
    input |> String.trim |> String.replace(~r/\d+/, "")
  end

  @doc """
  Compare the original string to a Kanye version, if they match someone is
  yelling.
  """
  def yelling?(input) do
    String.upcase(input) == input and Regex.match?(~r/[a-zA-ZЁёА-я]/, input)
  end
end

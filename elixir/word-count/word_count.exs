defmodule Words do
  @word_separators ~r/[^a-zA-ZЁёА-я0-9\-]+/

  @doc """
  Count the number of words in the sentence.

  Words are compared case-insensitively. Use regex ranges to split by anything
  that isn't wanted.
  """
  @spec count(String.t()) :: map
  def count(sentence) do
    sentence
    |> String.downcase
    |> String.split(@word_separators, trim: true)
    |> Enum.reduce(%{}, fn word, acc -> Map.update(acc, word, 1, &(&1 + 1)) end)
  end
end

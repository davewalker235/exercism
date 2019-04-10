defmodule Markdown do
  @moduledoc """
    Refactoring notes:
    * Rename variables to be more informative
    * Refactor deeply nested function calls to sequential pipes for easier comprehension
    * Favor case statement over nested if statements
    * Handle inline markdown before line level to avoid repetative searches
    * Favor string interpolation over binary concatenation operator "<>"
    * Avoid splitting and joining unneccessarily
  """

  @doc """
    Parses a given string with Markdown syntax and returns the associated HTML for that string.

    ## Examples

    iex> Markdown.parse("This is a paragraph")
    "<p>This is a paragraph</p>"

    iex> Markdown.parse("#Header!\n* __Bold Item__\n* _Italic Item_")
    "<h1>Header!</h1><ul><li><em>Bold Item</em></li><li><i>Italic Item</i></li></ul>"

    The regex for wrapping lists isn't ideal since it will fail if there are two
    separate lists. A better solution would be to iterate through the lines before
    joining and open or close the list by comparing lines.
  """
  @spec parse(String.t()) :: String.t()
  def parse(markdown) do
    output = markdown
    |> process_inline_tags
    |> String.split("\n")
    |> Enum.map(&process_line/1)
    |> Enum.join

    Regex.replace(~r/<li>(.+)<\/li>/, output, "<ul>\\0</ul>")
  end

  @doc """
    Use case statement to branch logic off first character of each line. Wrap
    each line according to it's markdown logic.
  """
  defp process_line(line) do
    case String.first(line) do
      "#" -> header_tag(line)
      "*" -> list_tag(line)
      _ -> "<p>#{line}</p>"
    end
  end

  @doc """
    Previously this split the markdown by carriage return and then processed each
    line separately. This is probably less effecient than handling the inline
    elements first globally and then formatting by line. Regex matching can be used
    to quickly find and replace the common underscore patterns in a more understandable
    way.
  """
  defp process_inline_tags(markdown) do
    Regex.replace(~r/(\_+)(.+?)\_+/, markdown, fn
      _match, "__", content -> "<strong>#{content}</strong>"
      _match, "_", content -> "<em>#{content}</em>"
    end)
  end

  @doc """
    Rather than splitting and reconstituting this line from parts, just split by
    the first space using the parts argument. This will leave two strings, the #
    notation can then be counted to determine level and the remaining string
    can be used as content
  """
  defp header_tag(line) do
    [header, line] = String.split(line, " ", parts: 2)
    level = String.length(header)
    "<h#{level}>#{line}</h#{level}>"
  end

  defp list_tag(line) do
    line = String.trim_leading(line, "* ")
    "<li>#{line}</li>"
  end
end

defmodule Gift.Question do
  defmacro __using__([]) do
    quote do
      defstruct text: "", answers: [], title: "", markup_language: :plain
    end
  end
end

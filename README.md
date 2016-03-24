# Giftex
Giftex is an Erlang and Elixir parser for the GIFT file format.

The [GIFT file format](https://docs.moodle.org/30/en/GIFT_format) is a simple
text file format for describing multiple choice, true false, short answer,
matching, missing word and numerical questions. It was developed by the
[Moodle Community](https://moodle.org/) for the Moodle LMS.

The parser itself is generated using the [Neotoma](https://github.com/seancribbs/neotoma)
PEG parser.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed as:

  1. Add giftex to your list of dependencies in `mix.exs`:

        def deps do
          [{:giftex, "~> 0.0.1"}]
        end

## Usage

Importing a file

        iex> {:ok, questions} = Giftex.import_file(path/to/file)
        {:ok, [%{type: :true_false_question, text: "Am I blue?", answers: [{:true, 100}]}, ...]}

Questions are returned as list of maps.

A question map has the following keys:

  * type -  An atom showing what type of question this is.
  * text -  The text of the question.
  * answers - A list of possible answers. Each answer is a tuple of the form
    {answer, percentage, feedback} except for matching questions which lists tuples
    of the correct matches.
  * title - A title for the question.
  * markup_language - One of :plain, :textile, :markdown, :html

## GIFT format

See [GIFT format](https://docs.moodle.org/30/en/GIFT_format) for detailed documentation.

At least one blank line is required between each question.

The question comes first followed by a {} pair containing a list of possible
  answers with an = indicating a correct answer and ~ indicating an incorrect or
  partially correct answer.

Comments are lines starting with // and are not imported.

### True/False Questions

The answer can be written as T, TRUE, F, FALSE.

    // A true or false question. Note the escaped = sign.
    ::Q1:: 1+1\=2 {T}

    {type: :true_false_question, text: "1+1=2", answers: {:true, 100}}

### Multiple Choice Questions

This question type has at least one incorrect or partially correct answer.
This example shows how to add feedback to answers with the # symbol.

    // multiple choice with specified feedback for right and wrong answers
    ::Q2:: What's between orange and green in the spectrum?{
      =yellow # right; good!
      ~red # wrong, it's yellow
      ~blue # wrong, it's yellow
    }

    {type: :multiple_choice_question, title: "Q2",
     text: "What's between orange and green in the spectrum?",
     answers: [{"yellow", 100, "right; good!"}, {"red", 0, "wrong, it's yellow"},
               {"blue", 0, "wrong, it's yellow"}]}

### Fill in The Blank Question

This question type has text after the answer list.
The place for the answer is replaced with an underscore.

    // fill-in-the-blank
    ::Q3:: Two plus {=two =2} equals four.

    {type: :fill_in_question, title: "Q3", text: "Two plus _ equals four.",
     answers: {"two", 100}, {"2", 100}}

### Matching Question

This is a question where pairs have to be matched.
Pairs are denoted by an arrow ->.
All answers must start with an =.

    // matching
    ::Q4:: Which animal eats which food? { =cat -> cat food =dog -> dog food }

    {type: :matching_question, title: "Q4", answers: {{"cat", "cat food"},{"dog", "dog food"}}}

### Numerical Range Question

This is a question where the answer must be within a numerical range.
There are two ways to specify a range. The parsed format just returns a tuple
with a maximum and minimum value.

    * #N:T where N is a value and T is a tolerance. The answer must be
      within N +/- T.

    * #M..N which is a range from M to N.

    // math range question with value and tolerance
    ::Q5:: What is a number from 1 to 5? {#3:2}

    // math range specified with interval end points
    ::Q6:: What is a number from 1 to 5? {#1..5}

    %{answers: [{{1, 5}, 100}], markup_language: :plain,
     text: "What is a number from 1 to 5?", title: "Q5", type: :numeric_question}

### Essay Question
  An essay question is denoted by not having any answers in the brackets.

    ::Q8:: Write about how great Elixir is.{}

    %{title: "Q8", type: :essay_question, text: "Write about how great Elixir is."}

### Short Answer Question
  A short answer question is denoted by only having correct answers
  (i.e. ones starting with =) in the answer list

    Who's buried in Grant's tomb?{=Grant =Ulysses S. Grant =Ulysses Grant}

    %{type: :short_answer_question, text: Who's buried in Grant's tomb?,
      answers: [{"Grant", 100}, {"Ulysses S. Grant", 100}, {"Ulysses Grant", 100}]}

### Description
  A description is just plain text with no brackets after it.

    A description here

    {type: :description, text: "A description here"}

### Commands

  A command is preceeded by a $.
  Whilst commands will parse correctly, nothing is yet implemented to process them.

    $CATEGORY=food
    {type: :command, command: "CATEGORY=food"}

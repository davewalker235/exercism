defmodule RobotSimulator do
  @directions [:north, :east, :south, :west]
  @instructions ~w(L R A)
  @type position() :: {integer, integer}
  @type robot() :: {direction :: atom, position :: {integer, integer}}

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: :north, :east, :south, :west
  """
  @spec create(direction :: atom, position :: {integer, integer}) :: {:error, String.t()} | robot()
  def create(direction \\ :north, position \\ {0, 0})
  def create(direction, _position) when direction not in @directions do
    {:error, "invalid direction"}
  end
  def create(direction, {x, y} = position) when is_integer(x) and is_integer(y) do
    {direction, position}
  end
  def create(_direction, _) do
    {:error, "invalid position"}
  end


  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot :: robot(), instructions :: String.t()) :: any
  def simulate(robot, instructions) when is_binary(instructions) do
    simulate(robot, String.codepoints(instructions))
  end
  def simulate(robot, []) do
    robot
  end
  def simulate(robot, [inst | instructions]) when inst in @instructions do
    {direction, position} = robot
    index = Enum.find_index(@directions, &(&1 === direction))
    robot = case inst do
      "L" ->
        {Enum.fetch!(@directions, index - 1), position}
      "R" ->
        {Enum.fetch!(Stream.cycle(@directions), index + 1), position}
      "A" ->
        {x, y} = position
        position = case direction do
          :north -> {x, y + 1}
          :east -> {x + 1, y}
          :south -> {x, y - 1}
          :west -> {x - 1, y}
        end
        {direction, position}
    end
    simulate(robot, instructions)
  end
  def simulate(_robot, _instructions) do
    {:error, "invalid instruction"}
  end

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot :: robot()) :: atom
  def direction({direction, _position}) do
    direction
  end

  @doc """
  Return the robot's position.
  """
  @spec position(robot :: robot()) :: {integer, integer}
  def position({_direction, position}) do
    position
  end
end

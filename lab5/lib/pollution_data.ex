defmodule PollutionData do
  @moduledoc false


  def importLinesFromCSV do
    lines = File.read!("pollution.csv") |> String.split("\r\n")
  end

  def convertOneLine(line) do
    [date, hour, latx, laty, value] = line |> String.split(",")
    date = String.split(date, "-") |> Enum.reverse()
    date = for x <- date, do: elem(Integer.parse(x), 0)
    date = :erlang.list_to_tuple(date)
    time = String.split(hour, ":")
    time = for x <- time, do: elem(Integer.parse(x), 0)
    time = :erlang.list_to_tuple(time ++ [0])
    datetime = {date, time}
    location = {elem(Float.parse(latx), 0), elem(Float.parse(laty), 0)}
    pollutionLevel = elem(Integer.parse(value), 0)
    %{:datetime => datetime, :location => location, :pollutionLevel => pollutionLevel}
  end

  def identifyStations() do
    separated_lines = for line <- PollutionData.importLinesFromCSV(), do: PollutionData.convertOneLine(line)
   # Enum.count(Enum.uniq_by(separated_lines, fn %{:datetime => _, :location => {x, y}, :pollutionLevel => _} -> {x, y} end))
    uniq_stations = Enum.uniq_by(separated_lines, fn %{:datetime => _, :location => {x, y}, :pollutionLevel => _} -> {x, y} end)
    for station <- uniq_stations, do: station[:location]
  end

  def addStations() do
    stations = PollutionData.identifyStations()
    for {x, y} <- stations, do: :pollution_gen_server.addStation("station_#{x}_#{y}", {x, y})
  end
# fn -> PollutionData.addStations end |> :timer.tc |> elem(0)

  def addValue() do
    separated_lines = for line <- PollutionData.importLinesFromCSV(), do: PollutionData.convertOneLine(line)
    for l <- separated_lines, do:
      :pollution_gen_server.addValue("station_#{elem(l[:location], 0)}_#{elem(l[:location], 1)}", l[:datetime], "PM10", l[:pollutionLevel])
  end
  # fn -> PollutionData.addValue end |> :timer.tc |> elem(0)

  # fn ->  :pollution_gen_server.getStationMean("station_20.06_49.986", "PM10")  end |> :timer.tc |> elem(0)
  # fn ->  :pollution_gen_server.getDailyMean({2017, 5, 3}, "PM10") end |> :timer.tc |> elem(0)
end

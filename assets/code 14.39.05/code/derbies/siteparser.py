# https://www.mirror.co.uk/sport/football/news/premier-league-derby-fixtures-dates-27249622
f = open("data.txt", "r")
lines = f.readlines()
rival_pairs = []
for line in lines:
    if line == "\n":
        continue
    else:
        parts = line.split(" - ")
        goodbit = parts[0]
        rival_pairs.append(goodbit)
replacements = {
    "Tottenham":"Tottenham Hotspur",
    "Man City": "Manchester City",
    "Man Utd": "Manchester United",
    "West Ham": "West Ham United",
    "Leicester": "Leicester City",
    "Brighton": "Brighton & Hove Albion"
}
fpairs = []
for i, pair in enumerate(rival_pairs):
    teams = pair.split(" vs ")
    if len(teams) == 1:
        print(f"error: string {teams} doesnt fit the pattern")
        continue
    team1 = teams[0]
    team2 = teams[1]
    if team1 in replacements.keys():
        team1 = replacements[team1]
    if team2 in replacements.keys():
        team2 = replacements[team2]
    newfpair = (team1, team2)
    if newfpair[::-1] in fpairs:
        continue
    else:
        fpairs.append( newfpair )
for i, (team1, team2) in enumerate(fpairs):
    print(f'{chr(i+65)} = c("{team1}", "{team2}"),')

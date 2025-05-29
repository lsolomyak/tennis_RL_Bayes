from pathlib import Path

# Paths
BASE_PATH = Path(__file__).parent.parent.parent.parent
DATA_PATH = BASE_PATH / "data" / "processed"
OUTPUT_PATH = Path.cwd() / "output"

# Player mappings
PLAYERS_2011 = {
    "Novak Djokovic": 1,
    "Serena Williams": 4,
    "Rafael Nadal": 8,
    "Roger Federer": 3
}

PLAYERS_2016 = {
    "Novak Djokovic": 1,
    "Serena Williams": 2,
    "Rafael Nadal": 4,
    "Roger Federer": 6
}

# Analysis parameters
DEFAULT_BINS = 20
MIN_SERVES_FOR_PLAYER = 100
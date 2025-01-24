import json
import sys

def explore_json(data, level=0):
    """
    Recursively explores the layers of a JSON structure.
    Prints keys and their nesting level.

    Args:
        data (dict or list): The JSON data to explore.
        level (int): Current depth level in the structure.
    """
    indent = "  " * level
    if isinstance(data, dict):
        for key, value in data.items():
            print(f"{indent}Level {level}: Key -> {key}")
            explore_json(value, level + 1)
    elif isinstance(data, list):
        print(f"{indent}Level {level}: List with {len(data)} items")
        for i, item in enumerate(data[:5]):  # Limit output for large lists
            print(f"{indent}  [Item {i}]")
            explore_json(item, level + 1)
        if len(data) > 5:
            print(f"{indent}  ... and {len(data) - 5} more items")
    else:
        print(f"{indent}Level {level}: Value -> {data}")

def load_json_file(file_path):
    """
    Loads a JSON file and returns its contents.
    
    Args:
        file_path (str): Path to the JSON file.

    Returns:
        dict or list: Parsed JSON data.
    """
    try:
        with open(file_path, 'r', encoding='utf-8') as file:
            return json.load(file)
    except FileNotFoundError:
        print("Error: File not found. Please check the file path.")
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON format. {e}")
    except Exception as e:
        print(f"An unexpected error occurred: {e}")
    return None

def main():
    if len(sys.argv) < 2:
        print("Usage: python explore_json.py <path_to_json_file>")
        sys.exit(1)

    file_path = sys.argv[1]
    data = load_json_file(file_path)
    if data:
        print("\nExploring JSON structure:")
        explore_json(data)

if __name__ == "__main__":
    main()


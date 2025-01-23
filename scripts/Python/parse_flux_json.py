import os
import json
import csv
from pathlib import Path

def parse_flux_json(input_file, output_dir):
    """
    Parse the complex JSON structure and export it to CSV files.
    - Metadata (header) is saved as a separate CSV.
    - Time-series data (data) is saved as another CSV.
    """
    # Ensure input file exists
    if not os.path.isfile(input_file):
        print(f"Error: File '{input_file}' does not exist.")
        return

    # Load JSON data
    with open(input_file, 'r') as f:
        try:
            data = json.load(f)
        except json.JSONDecodeError as e:
            print(f"Error: Failed to decode JSON - {e}")
            return

    # Ensure output directory exists
    os.makedirs(output_dir, exist_ok=True)

    # Parse datasets
    datasets = data.get("datasets", [])
    if not isinstance(datasets, list):
        print("Error: 'datasets' key is missing or not a list.")
        return

    for dataset in datasets:
        for key, values in dataset.items():
            # Create subfolder for each dataset
            dataset_dir = os.path.join(output_dir, key)
            os.makedirs(dataset_dir, exist_ok=True)

            # Process 'header' data
            header_data = values.get("reps", {}).get("REP_1", {}).get("header", {})
            if header_data:
                write_dict_to_csv([header_data], os.path.join(dataset_dir, f"{key}_header.csv"))

            # Process 'data'
            time_series_data = values.get("reps", {}).get("REP_1", {}).get("data", {})
            if time_series_data:
                write_timeseries_to_csv(time_series_data, os.path.join(dataset_dir, f"{key}_data.csv"))

            # Process 'summary'
            summary_data = values.get("reps", {}).get("REP_1", {}).get("summary", {})
            if summary_data:
                write_dict_to_csv([summary_data], os.path.join(dataset_dir, f"{key}_summary.csv"))

            # Process 'footer'
            footer_data = values.get("reps", {}).get("REP_1", {}).get("footer", {})
            if footer_data:
                write_dict_to_csv([footer_data], os.path.join(dataset_dir, f"{key}_footer.csv"))

    print(f"Parsing completed. Results saved in '{output_dir}'")


def write_dict_to_csv(data, output_file):
    """
    Write a list of dictionaries to a CSV file.
    """
    try:
        with open(output_file, 'w', newline='') as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=data[0].keys())
            writer.writeheader()
            writer.writerows(data)
        print(f"Saved: {output_file}")
    except Exception as e:
        print(f"Error writing to CSV: {e}")


def write_timeseries_to_csv(data, output_file):
    """
    Write time-series data to a CSV file.
    Each key in 'data' is a column.
    """
    try:
        with open(output_file, 'w', newline='') as csvfile:
            # Transpose time-series data
            keys = data.keys()
            rows = zip(*[data[k] for k in keys])

            writer = csv.writer(csvfile)
            writer.writerow(keys)  # Write headers
            writer.writerows(rows)  # Write rows
        print(f"Saved: {output_file}")
    except Exception as e:
        print(f"Error writing time-series to CSV: {e}")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Parse flux JSON file to CSV.")
    parser.add_argument("input_file", help="Path to the JSON file.")
    parser.add_argument("output_dir", help="Directory to save CSV files.")

    args = parser.parse_args()
    parse_flux_json(args.input_file, args.output_dir)


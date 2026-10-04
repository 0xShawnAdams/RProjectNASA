from pathlib import Path
import zipfile
import requests

url = "https://phm-datasets.s3.amazonaws.com/NASA/5.+Battery+Data+Set.zip"

output_dir = Path("data/nasa_battery")
output_dir.mkdir(parents=True, exist_ok=True)

zip_path = output_dir / "NASA_Battery_Data_Set.zip"

print(f"Downloading:\n{url}\n")

with requests.get(url, stream=True, timeout=(30, 600)) as response:
    response.raise_for_status()

    total = int(response.headers.get("Content-Length", 0))
    downloaded = 0

    with zip_path.open("wb") as file:
        for chunk in response.iter_content(chunk_size=1024 * 1024):
            if chunk:
                file.write(chunk)
                downloaded += len(chunk)

                if total:
                    percent = downloaded / total * 100
                    print(
                        f"\rDownloaded: {downloaded / 1024 / 1024:.1f} MB "
                        f"of {total / 1024 / 1024:.1f} MB ({percent:.1f}%)",
                        end="",
                        flush=True,
                    )

print("\n\nChecking archive...")

if not zipfile.is_zipfile(zip_path):
    preview = zip_path.read_bytes()[:500].decode("utf-8", errors="replace")
    zip_path.unlink(missing_ok=True)
    raise RuntimeError(
        "The download was not a valid ZIP archive.\n"
        f"Server response preview:\n{preview}"
    )

with zipfile.ZipFile(zip_path, "r") as archive:
    mat_files = [
        item for item in archive.namelist()
        if item.lower().endswith(".mat")
    ]

    print(f"Valid ZIP archive: {len(mat_files)} MATLAB files found.")
    print("\nFirst files in the archive:")
    for name in mat_files[:20]:
        print(" ", name)

    archive.extractall(output_dir)

print(f"\nExtracted to: {output_dir.resolve()}")

import json
import csv
import requests
import re
import datetime
from os import listdir, mkdir, path
from shutil import copyfile, copytree
from collections import defaultdict

"""
Usage:
Insert your API key in API_TOKEN, pull the CSV from LearnIT into the current folder, run the script,
select the correct CSV file, and you're off to the races!
"""

API_URL = "https://app.codejudge.net/api/"
API_TOKEN = "<YOUR TOKEN HERE>"

model = {"grantType": "refresh_token", "refreshToken": API_TOKEN}
jwtReq = requests.post(API_URL + "auth/token", data=json.dumps(model))
jwtReq.raise_for_status()
token = jwtReq.json()["accessToken"]

headers = {
    "Authorization": f"Bearer {token}",
    "Accept": "application/json",
    "User-Agent": "Functional Programming Grading Script v1"
}

direc = sorted([*filter(lambda x: x.startswith("Grades")
                        and x.endswith(".csv"), listdir())])
cache = [*filter(lambda x: x.endswith(".json"), listdir(".cache"))]
if not path.isdir(".files-cache"):
    mkdir(".files-cache")
subCache = listdir(".files-cache")
choice = ""
while (not choice.isdigit() or len(direc) < int(choice)):
    print("Choose a file:")
    for i, f in enumerate(direc):
        print(f"{i+1}) {f}")
    choice = input("> ")

choice = direc[int(choice) - 1]


def getFile(submission):
    content = ""
    if f"{submission}" in subCache or f"{submission}.fs" in subCache:
        if f"{submission}.fs" in subCache:
            with open(f".files-cache/{submission}.fs", "r") as fs:
                content = "\n".join(fs.readlines())
        else:
            for fileName in listdir(f".files-cache/{submission}"):
                with open(f".files-cache/{submission}/{fileName}", "r") as fs:
                    content += "\n".join(fs.readlines())
    else:
        r = requests.get(
            f"https://itu.codejudge.net/api/submissions/{submission}/file-links", headers=headers)
        files = r.json()
        if len(files) == 1:
            fileID = files[0]["id"]
            content = requests.get(
                f"https://itu.codejudge.net/api/submissions/{submission}/file/{fileID}", headers=headers).json()["content"]
            with open(f".files-cache/{submission}.fs", "w") as fs:
                fs.write(content)
        else:
            mkdir(f".files-cache/{submission}")
            for file in files:
                fileID = file["id"]
                res = requests.get(
                    f"https://itu.codejudge.net/api/submissions/{submission}/file/{fileID}", headers=headers).json()
                with open(f".files-cache/{submission}/{res['name']}", "w") as fs:
                    fs.write(res["content"])
                content += res["content"]

    if "mutable" in content or "<-" in content:
        print("Found a code smell (mutability) in", submission)
    elif "Exception" in content:
        print("Found a code smell (exceptions) in", submission)


def checkName(submission, username):
    queryName = f"query-{submission}.json"
    if queryName in cache:
        with open(f".cache/{queryName}") as qf:
            j = json.load(qf)
    else:
        endpoint = f"https://itu.codejudge.net/api/submissions?submissionId={submission}&filterStart=&filterEnd=&exerciseError=false&page=0&pageSize=20&courseId=1303"
        j = requests.get(endpoint, headers=headers).json()
        with open(f".cache/{queryName}", "w") as qf:
            json.dump(j, qf)
    gottenName = j["submissions"][0]["userDisplayName"]
    if username != gottenName:
        print(
            f"Submitter '{gottenName}' does not entirely match expected '{username}'!")


submissionNumbers = set()
exerciseIDs = defaultdict(set)
red = []
yellow = []

now = datetime.datetime.now().strftime("%A, %d %B %Y, %H:%M")

with open(choice, "r") as f, open("graded-" + choice, "w") as w:
    rd = csv.reader(f)
    wr = csv.writer(w, delimiter=",")
    for i, row in enumerate(rd):
        if i == 0:  # Write the header line
            wr.writerow(row)
            continue
        m = re.search("\\d{7}", row[9])
        if not m:  # If not parseable, alert attendant
            print(
                f"Skipped {row[1]} due to missing submission (Content: '{row[9]}')")
        elif not row[5]:  # If parseable, and no grade yet
            subID = m[0]
            if f"{subID}.json" in cache:
                with open(f".cache/{subID}.json") as f2:
                    j = json.load(f2)
            else:
                r = requests.get(
                    "https://itu.codejudge.net/api/test-group-runs",
                    params={"submissionId": subID},
                    headers=headers)
                try:
                    j = r.json()
                    with open(f".cache/{subID}.json", "w") as f2:
                        f2.write(r.text)
                except requests.exceptions.JSONDecodeError:
                    print(f"Something went wrong on {subID}: '{r}'")

            if "error" in j:
                score = f"ERROR: {j['error']}"
            else:
                greenSuccess = True
                for t in j:
                    if "Green" in t["testGroup"]["name"] and (t["result"] == "testFailed" or t["result"] == "compileError"):
                        greenSuccess = False
                    if "Red" in t["testGroup"]["name"] and t["result"] == "succeeded":
                        red.append(subID)
                    if "Yellow" in t["testGroup"]["name"] and t["result"] == "succeeded":
                        yellow.append(subID)

                # if len(j) >= 3 and (j[2]["result"] == "succeeded" or len(j[2]["testRuns"]) > 1):
                #     red.append(subID)
                if greenSuccess:
                    # Green is green, mission complete
                    score = "2.00"
                    exerciseIDs[j[0]["testGroup"]["exerciseId"]].add(subID)
                else:
                    # Something was submitted, throw them a bone
                    # We also notify attendant to ensure "an attempt was made"
                    print(
                        f"Awarded score '1.0' to {row[1]}, double check quality: {subID}")
                    score = "1.00"
                    exerciseIDs[j[0]["testGroup"]["exerciseId"]].add(subID)
            row[5] = score
            row[10] = now  # format like "Monday, 6 February 2023, 16:51"
            wr.writerow(row)
        if m:
            subID = m[0]
            # Check if name on submission matches name in CSV
            checkName(subID, row[1])
            getFile(subID)  # Also get the submission itself
            if subID in submissionNumbers:
                print(
                    subID, f"was already submitted by someone other than {row[1]}!")
            submissionNumbers.add(subID)

# Check if submission is for wrong exercise
if len(exerciseIDs) > 1:
    print("Submissions from wrong exercise in the batch!")
    print(exerciseIDs)

if not path.isdir(".red-files"):
    mkdir(".red-files")
for red_file in set(red):
    if path.isfile(f".files-cache/{red_file}.fs"):
        copyfile(f".files-cache/{red_file}.fs", f".red-files/{red_file}.fs")
    else:
        copytree(f".files-cache/{red_file}", f".red-files/{red_file}")

if not path.isdir(".yellow-files"):
    mkdir(".yellow-files")
for yellow_file in set(yellow):
    if path.isfile(f".files-cache/{yellow_file}.fs"):
        copyfile(f".files-cache/{yellow_file}.fs",
                 f".yellow-files/{yellow_file}.fs")
    else:
        copytree(f".files-cache/{yellow_file}", f".yellow-files/{yellow_file}")

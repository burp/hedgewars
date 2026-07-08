import subprocess
import time
import sys
import os
import glob

def get_latest_crash_report():
    paths = [
        os.path.expanduser("~/Library/Logs/DiagnosticReports/*.crash"),
        os.path.expanduser("~/Library/Logs/DiagnosticReports/*.ips")
    ]
    files = []
    for path in paths:
        files.extend(glob.glob(path))
    if not files:
        return None
    # return the most recently modified file
    return max(files, key=os.path.getmtime)

def main():
    print("[Verification] Starting Hedgewars headlessly with official connection...")
    
    # Run the frontend with offscreen platform and connection string
    p = subprocess.Popen(
        ["./build/Hedgewars.app/Contents/MacOS/hedgewars", "-platform", "offscreen", "hwplay://official.hedgewars.org:46667"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    
    # Wait for 10 seconds to allow network resolution and connection setup
    time.sleep(10)
    
    ret = p.poll()
    if ret is None:
        print("[Verification] Application is still running fine after 10 seconds. Connection setup succeeded without crash!")
        p.terminate()
        try:
            p.wait(timeout=2)
        except subprocess.TimeoutExpired:
            p.kill()
        sys.exit(0)
    else:
        print(f"[Verification] Application crashed / exited early with code: {ret}")
        stdout, stderr = p.communicate()
        print("[Verification] Stdout:\n", stdout)
        print("[Verification] Stderr:\n", stderr)
        
        # Look for system crash logs
        crash_file = get_latest_crash_report()
        if crash_file:
            print(f"\n[Verification] Found system crash report: {crash_file}\n")
            try:
                with open(crash_file, 'r') as f:
                    print(f.read())
            except Exception as e:
                print(f"[Verification] Could not read crash file: {e}")
        else:
            print("\n[Verification] No system crash report found.")
            
        sys.exit(ret if ret != 0 else 1)

if __name__ == '__main__':
    main()

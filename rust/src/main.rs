
use subprocess::PopenError;
use subprocess::Redirection;

use std::time::Duration;
use subprocess::Exec;

// global settings
const hash_prefix : &str = ":";


// error handling

#[derive(Debug)]
enum Err {
    Popen(PopenError),
    IO(std::io::Error),
    Git(String),
}

impl std::convert::From<PopenError> for Err {
    fn from(x : PopenError) -> Self {
        Err::Popen(x)
    }
}

impl std::convert::From<std::io::Error> for Err {
    fn from(x : std::io::Error) -> Self {
        Err::IO(x)
    }
}


impl <T> std::convert::Into<std::result::Result<T, Err>> for Err {
    fn into(self) -> std::result::Result<T, Err> {
        Result::Err(self)
    }
}


#[derive(Debug)]
struct GitBase {
    dir : String,
    branch : String,
}

#[derive(Debug)]
struct GitStatus {
    branch : String,
    ahead : u32,
    behind : u32,
    staged: u32,
    conflicts: u32,
    changed : u32,
    untracked : u32,
}

fn run_command(cmd : &str) -> Result<(String, String), Err> {
    
    let mut p = Exec::shell(cmd)
        .stdout(Redirection::Pipe)
        .stderr(Redirection::Merge)
        .popen()?;

    if let Some(status) = p.wait_timeout(Duration::new(1, 0))? {
        let (stdout, stderr) = p.communicate(None)?;
        let (stdout, stderr) = (stdout.unwrap_or("".to_string()), stderr.unwrap_or("".to_string()));
        let (stdout, stderr) = (stdout.trim(), stderr.trim());
        return Ok((stdout.to_string(), stderr.to_string()));
    } else {
        p.kill()?;
        p.wait()?;
        println!("process killed");
    }


    Ok((String::from(""), String::from("")))
}


fn get_git_base(path : &str) -> Result<GitBase, Err> {
    let (stdout, stderr) = run_command("git rev-parse --show-toplevel --symbolic-full-name HEAD")?;

    if stderr.contains("fatal: Not a git repository") {
        return Err::Git("no git repo".to_string()).into();
    }

    let words = stdout.split("\n").collect::<Vec<_>>();
    if words.len() < 2 {
        return Err::Git("unexpected output".to_string()).into();
    }

    let path = words[0];
    let ref_name = words[1].trim();
    let branch = &ref_name[11..];

    let base = GitBase { 
        dir: path.to_string(), 
        branch: branch.to_string() 
    };

    Ok(base)
}


fn git_status(path : &str) -> Result<GitStatus, Err> {
    let base = get_git_base(path)?;
 
    let (gitstatus_stdout, gitstatus_stderr) = run_command("git status --porcelain")?;
    if gitstatus_stderr.contains("fatal") {
        return Err::Git("".to_string()).into()
    }

    let lines = gitstatus_stdout.split("\n").collect::<Vec<_>>();

    fn count(lines : &Vec<&str>, idx : usize, status_str : &str) -> u32 {
        let mut count : u32 = 0;
        for line in lines {
            let substr = line.get(idx..);
            if let Some(substr) = substr {
                if substr.starts_with(status_str) {
                    count+=1;
                }
            }
        }
        count
    }


    let staged = count(&lines, 0, "M") + count(&lines, 0, "A");
    let conflicts = count(&lines, 0, "UU");
    let changed = count(&lines, 1, "M");
    let untracked = count(&lines, 0, "??");
    
    let mut ahead = 0;
    let mut behind = 0;
    let mut branch = base.branch;

    if branch.len() == 0 {
        // not on any branch
        let (hash, _) = run_command("git rev-parse --short HEAD")?;
        branch = hash_prefix.to_string() + &hash;
    }
    else {
        // check cached remote status
        let (remote_name, _) = run_command(&format!("git config branch.{}.remote", branch))?;
        let mut remote_name = remote_name;

        if !remote_name.is_empty() {
            let (merge_name, _) = run_command(&format!("git config branch.{}.merge", branch))?;
            let remote_ref = if remote_name == "." {
                merge_name
            } else {
                format!("refs/remotes/{}/{}", remote_name, merge_name.get(11..).unwrap_or(""))
            };

            let (rev_git, _) = run_command(&format!("git rev-list --left-right {}...HEAD", remote_ref))?;
            println!("git rev-list --left-right {}...HEAD", remote_ref);
            println!("{}", rev_git);
            // TODO?: failback to local

            let lines = rev_git.split("\n").collect::<Vec<_>>();
            ahead = count(&lines, 0, ">");
            behind = count(&lines, 0, "<");

        }
    }

    Ok(GitStatus {
        branch,
        ahead,
        behind,
        staged,
        conflicts,
        changed,
        untracked,
    })
}


fn main() -> Result<(), Err> {
    println!("status master 2 0 0 0 0 0");

    println!("{:?}", get_git_base("."));
    println!("{:?}", git_status("."));

    let status = git_status(".")?;
    
    print!("status");
    print!(" {}", status.branch);
    print!(" {}", status.ahead);
    print!(" {}", status.behind);
    print!(" {}", status.staged);
    print!(" {}", status.conflicts);
    print!(" {}", status.changed);
    print!(" {}", status.untracked);
    println!("");

    Ok(())
}

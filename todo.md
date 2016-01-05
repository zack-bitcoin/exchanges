instead of checking each exchange one at a time linearly, we should use a seperate process to check each exchange and collect the results at the end.

Instead of deleting the CSV page and re-writing it each time, we should remove and re-write a single line at a time. That way if a different program is simultaniously reading from the file, it can read updates as soon as possible and wont wait around staring at half-filled files.

An OTP gen_server should control each of the files.

We need a way to load private keys from each exchange into a script so that we can sign post requests and do trades. This will involve setting up an account on every exchange.
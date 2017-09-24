print("no threads yet");

thread sergei
{
	let x = 1000000;
	while x > 0
	{
		x = x - 1;
		print("thread sergei");
	}
	print("sergei finished")
}
let x = 1000000;
while x > 0
{
	x = x - 1;
	print("thread main");
}

print("main finished");

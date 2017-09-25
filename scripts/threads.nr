print("no threads yet");

thread sergei
{
	print("thread child");
	let x = 1;
	while x > 0
	{
		x = x - 1;
		print("thread child");
	}
	print("child finished")
}
let x = 1;
while x > 0
{
	x = x - 1;
}

print("main thread finished");

fn test()
{
	thread sergei_fn
	{
		print("fn thread child");
		let x = 1;
		while x > 0
		{
			x = x - 1;
			print("fn thread child");
		}
		print("fn child finished")
	}
}

test()

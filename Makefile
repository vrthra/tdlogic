all: todo

todo: todo.pl
	gplc --no-susp-warn --no-redef-error --no-singl-warn --no-top-level --no-debugger todo.pl

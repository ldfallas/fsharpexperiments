open System.Windows.Forms
open System.Drawing

let f = new Form()
f.Size <- new Size(300,300)

let c = new Cursor(System.Environment.GetCommandLineArgs().[1])
f.Cursor <- c

Application.Run(f)

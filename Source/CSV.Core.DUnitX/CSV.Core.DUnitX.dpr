program CSV.Core.DUnitX;

{$UNDEF MODO_CONSOLA}

{$IFDEF MODO_CONSOLA}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF MODO_CONSOLA}
  DUnitX.Loggers.Console,
  {$ELSE}
  VCL.Forms,
  DUnitX.Loggers.GUI.VCL,
  {$ENDIF }
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  CSV.Core.Model.DUnitX in 'Src\CSV.Core.Model.DUnitX.pas',
  CSV.Core.ViewModel.DUnitX in 'Src\CSV.Core.ViewModel.DUnitX.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
{$IFNDEF MODO_CONSOLA}
  Application.Initialize;
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  Application.Run;
{$ELSE}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
end.

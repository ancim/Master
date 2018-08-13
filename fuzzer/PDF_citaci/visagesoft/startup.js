// Static stamps
// Stamp color format is windows BGR
// AddStampAnnotation(Text: String;baseColor, gradientStartColor, gradientEndColor: cardinal) returns boolean;
// true added, stamp  exists
App.SetStampGroup("Static");
App.AddStampAnnotation("Approved");
App.AddStampAnnotation("Completed" );
App.AddStampAnnotation("Confidential" );
App.AddStampAnnotation("Draft" );
App.AddStampAnnotation("Final" );
App.AddStampAnnotation("For Comment" );
App.AddStampAnnotation("For Public Release" );
App.AddStampAnnotation("Information Only" );
App.AddStampAnnotation("Not Approved", 0x050f91, 0xebedfa , 0xd0d0ff);
App.AddStampAnnotation("Not For Public Release");
App.AddStampAnnotation("Preliminary Results");
App.AddStampAnnotation("Void");

//App.AddStampAnnotationFromSVG("D:\\Develop\\products\\printers\\VSPDFPrinterDriver\\v7\\design\\stamps\\paid.svg");

// Dynamic stamps
App.SetStampGroup("Dynamic");
App.AddStampAnnotation("Approved\nBy ${USER} at ${DATE dd mmm yy hh:nn}");
App.AddStampAnnotation("Confidential\nBy ${USER} at ${DATE dd mmm yy hh:nn}");
App.AddStampAnnotation("Received\nBy ${USER} at ${DATE dd mmm yy hh:nn}");
App.AddStampAnnotation("Reviewed\nBy ${USER} at ${DATE dd mmm yy hh:nn}", 0x1C6A41, 0xebf7f2, 0xc6e0c7);
App.AddStampAnnotation("Revised\nBy ${USER} at ${DATE dd mmm yy hh:nn}", 0x1C6A41, 0xebf7f2, 0xc6e0c7);

// Known paper sizes
// AddPaperSize(width, height, APP_UNIT_POINT or APP_UNIT_INCH or APP_UNIT_MM) returns boolean
// true added, false paper name exists

App.AddPaperSize('A0', 2384, 3370, APP_UNIT_POINT);
App.AddPaperSize('A1', 1684, 2384);
App.AddPaperSize('A2', 1190, 1684);
App.AddPaperSize('A3', 842, 1190);
App.AddPaperSize('A4', 8.27, 11.69, APP_UNIT_INCH);
App.AddPaperSize('A5', 420, 595);
App.AddPaperSize('A6', 298, 420);
App.AddPaperSize('A7', 209, 298);
App.AddPaperSize('Letter', 612, 792);
App.AddPaperSize('Legal', 612, 1008);
App.AddPaperSize('Ledger ', 792, 1224);
App.AddPaperSize('Tabloid', 1224, 792);
App.SetDefaultPaperSize('Letter');



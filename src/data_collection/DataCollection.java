package data_collection;

import com.theeyetribe.clientsdk.GazeManager;
import com.theeyetribe.clientsdk.IConnectionStateListener;
import com.theeyetribe.clientsdk.IGazeListener;
import com.theeyetribe.clientsdk.ITrackerStateListener;
import com.theeyetribe.clientsdk.data.GazeData;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.Button;
import javafx.scene.control.ColorPicker;
import javafx.scene.control.Label;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;
import javafx.stage.Screen;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

import java.io.File;
import java.text.SimpleDateFormat;

import java.io.IOException;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.Date;
import javax.swing.JOptionPane;

/**
 * Created by Phong on 16/11/2016.
 */
public class DataCollection extends Application {

    private static String experimenterName = "NO-NAME";

    private int LINE_WIDTH = 5;
    private double before_x = 0;
    private double before_y = 0;
    private double offsetX = 0;
    private double offsetY = 0;
    private double rawX = 0;
    private double rawY = 0;
    private ColorPicker colorPicker;
    private int X_SCREEN_MAX = (int)Screen.getPrimary().getVisualBounds().getMaxX();
    private int Y_SCREEN_MAX = (int)Screen.getPrimary().getVisualBounds().getMaxY();
    private boolean drawflag = true;
    private long counter = 0L;
    private boolean writingFlag = false;
    private BufferedWriter writer;
    private double canvasWidth;
    private double canvasHeight;


    @Override
    public void start(Stage primaryStage) throws IOException {
        //Gaze
        final GazeManager gm = GazeManager.getInstance();
        gm.addConnectionStateListener(new IConnectionStateListener() {
            @Override
            public void onConnectionStateChanged(boolean b) {
                System.out.println("State of Connection with EyeTribe Device: " + b);
            }
        });
        gm.addTrackerStateListener(new ITrackerStateListener() {
            @Override
            public void onTrackerStateChanged(int i) {
                System.out.println("State of Tracker Listener: " + i);
            }
        });

        //Draw the shape of SAT reading test arrangement on Khan Academy
        Rectangle rect1 = new Rectangle(0,30,X_SCREEN_MAX,200);
        rect1.setFill(Color.TRANSPARENT);
        rect1.setStroke(Color.BLACK);
        rect1.setStrokeWidth(10);

        Rectangle rect2 = new Rectangle(0,250,X_SCREEN_MAX/2 - 50,Y_SCREEN_MAX-300);
        rect2.setFill(Color.TRANSPARENT);
        rect2.setStroke(Color.BLACK);
        rect2.setStrokeWidth(10);

        Rectangle rect3 = new Rectangle(X_SCREEN_MAX/2 + 50,250,X_SCREEN_MAX/2 - 50,Y_SCREEN_MAX-300);
        rect3.setFill(Color.TRANSPARENT);
        rect3.setStroke(Color.BLACK);
        rect3.setStrokeWidth(10);

        // Initialize Canvas
        primaryStage.setMaximized(true);
        Canvas canvas = new Canvas(X_SCREEN_MAX,Y_SCREEN_MAX-50);

        // Initialize Graphic Context
        final GraphicsContext graphicsContext = canvas.getGraphicsContext2D();
        // Prepare the graphic Context
        initDraw(graphicsContext);

        // Create a start button to start collecting data
        Button startButton = new Button("Start Data Collection");
        Label label = new Label();

        startButton.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {
                writingFlag = true;
                // Add the timestamp into the file's name
                SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd_HHmmss");
                Date date = new Date();
                String timeNow = sdf.format(date);
                // Initialize the writer which will wtite data to file
                String fileName = "EyeGazeData/" + timeNow + "_" + experimenterName + "_GazeData.csv";
                try {
                    writer = new BufferedWriter(new FileWriter(fileName));
                } catch (IOException e) {
                    e.printStackTrace();
                    label.setText("There is an error with trying to write to file.");
                }
                System.out.println("Start Writing To File");
                startButton.setDisable(true);
                label.setText("Collecting Data Now...");
            }
        });

        // Wrap drawing code into Platform.runLater for stability of thread synchronization.
        Platform.runLater(new Runnable() {
            @Override
            public void run() {


                //Check if folder exists, if not, create one
                File directory = new File("EyeGazeData");
                if (! directory.exists()){
                    directory.mkdir();
                }

                // Add Listener
                gm.addGazeListener(new IGazeListener() {
                    @Override
                    public void onGazeUpdate(GazeData gazeData) {
                        final float RECT_RADIUS = 100;
                        //System.out.println(gazeData.rawCoordinates.toString());
                        double x = gazeData.rawCoordinates.x;
                        double y = gazeData.rawCoordinates.y;
                        String timeStamp = gazeData.timeStampString;


                        rawX = x;
                        rawY = y;

                        //System.out.println(offsetX + "," + offsetY);

                        x = x + offsetX;
                        y = y + offsetY;

                        // TO-DO: write to file.
                        String stringX = Double.toString(x);
                        String stringY = Double.toString(y);

                        if (writingFlag) {
                            try {
                                writer.write(timeStamp + "," + stringX + "," + stringY + "\n");
                                counter++;
                                if(counter % 100 == 0) {
                                    writer.flush();
                                }
                            } catch (IOException e) {
                                e.printStackTrace();
                                label.setText("There is an error with trying to write to file.");
                            }
                        }


                        if(x == 0 && y == 0)
                        {
                            return;
                        }
                        if(x < 0 || x > graphicsContext.getCanvas().getWidth() || y < 0 || y > graphicsContext.getCanvas().getHeight())
                        {
                            //System.out.println("see outside");
                            return;
                        }
                        //graphicsContext.setStroke(colorPicker.getValue());
                        graphicsContext.setFill(colorPicker.getValue());
                        //graphicsContext.beginPath();
                        graphicsContext.clearRect(before_x-RECT_RADIUS,before_y-RECT_RADIUS,before_x+RECT_RADIUS,before_y+RECT_RADIUS);
//                graphicsContext.strokeRect(
//                        0,              //x of the upper left corner
//                        0,              //y of the upper left corner
//                        canvas.getWidth(),    //width of the rectangle
//                        canvas.getHeight());  //height of the rectangle
                        graphicsContext.fillOval(x-RECT_RADIUS,y-RECT_RADIUS,RECT_RADIUS,RECT_RADIUS);
                        before_x = x;
                        before_y = y;
                    }
                });
                gm.activate();
                Group root = new Group();
                root.setOnMouseClicked(new EventHandler<MouseEvent>() {
                    @Override
                    public void handle(MouseEvent event) {
                        double clickX = event.getSceneX();
                        double clickY = event.getSceneY();
                        offsetX = clickX - rawX;
                        offsetY = clickY - rawY;
                    }
                });

                VBox vBox = new VBox();
                HBox hBox = new HBox();
                hBox.getChildren().addAll(startButton);
                hBox.getChildren().addAll(label);
                vBox.getChildren().addAll(hBox,canvas);
                root.getChildren().addAll(rect1, rect2, rect3, vBox);
                Scene scene = new Scene(root, 300, 300);
                primaryStage.setTitle("Eye Gaze Visualization");
                primaryStage.setScene(scene);
                primaryStage.show();
                primaryStage.setOnCloseRequest(new EventHandler<WindowEvent>() {
                    @Override
                    public void handle(WindowEvent event) {
                        if (writingFlag){
                            try {
                                writer.flush();
                                writer.close();
                                writingFlag = false;
                            } catch (IOException e) {
                                e.printStackTrace();
                                label.setText("There is an error with Stream Writer.");
                            }
                        }

                        System.out.println("Total data collected: " + counter);
                        Platform.exit();
                        System.exit(0);
                    }
                });
            }
        });
    }

    public static void main(String[] args) {
        experimenterName = JOptionPane.showInputDialog("Please input your full name.");
        experimenterName = experimenterName.replaceAll(" ", "_");
        launch(args);
    }

    private void initDraw(GraphicsContext gc){

        colorPicker = new ColorPicker(Color.BLACK);
        canvasWidth = gc.getCanvas().getWidth();
        canvasHeight = gc.getCanvas().getHeight();
        gc.setFont(Font.font("Arial", FontWeight.BOLD, 52));
        System.out.println("Screen Width: " + canvasWidth + "\nScreen Height: " + canvasHeight);

    }
}

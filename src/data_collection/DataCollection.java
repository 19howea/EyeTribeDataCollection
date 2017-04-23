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
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.Button;
import javafx.scene.control.ColorPicker;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.stage.Screen;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

import java.io.File;
import java.text.SimpleDateFormat;

import java.io.IOException;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.Date;

/**
 * Created by Phong on 16/11/2016.
 */
public class DataCollection extends Application {

    final String experimenterName = "Andrew_Howe";

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


    @Override
    public void start(Stage primaryStage) throws IOException {
        //Gaze
        final GazeManager gm = GazeManager.getInstance();
        gm.addConnectionStateListener(new IConnectionStateListener() {
            @Override
            public void onConnectionStateChanged(boolean b) {
                System.out.println("" + b);
            }
        });
        gm.addTrackerStateListener(new ITrackerStateListener() {
            @Override
            public void onTrackerStateChanged(int i) {
                System.out.println(i);
            }
        });

        // Initialize Canvas
        primaryStage.setMaximized(true);
        Canvas canvas = new Canvas(X_SCREEN_MAX,Y_SCREEN_MAX-50);
        // Initialize Clear Button
        final GraphicsContext graphicsContext = canvas.getGraphicsContext2D();
        Button clear = new Button("Clear");
        clear.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {

                graphicsContext.setStroke(Color.BLACK);
                graphicsContext.clearRect(0,0,canvas.getWidth(),canvas.getHeight());
                graphicsContext.strokeRect(
                        0,              //x of the upper left corner
                        0,              //y of the upper left corner
                        canvas.getWidth(),    //width of the rectangle
                        canvas.getHeight());  //height of the rectangle
                graphicsContext.setStroke(colorPicker.getValue());
                System.out.println("Canvas Clear");
            }
        });
        initDraw(graphicsContext);

        // Add the timestamp into the file's name
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd_HHmmss");
        Date date = new Date();
        String timeNow = sdf.format(date);

        //Check if folder exists, if not, create one
        File directory = new File("EyeGazeData");
        if (! directory.exists()){
            directory.mkdir();
        }

        // Initialize the writer which will wtite data to file
        String fileName = "EyeGazeData/" + timeNow + "_" + experimenterName + "_GazeData.csv";
        BufferedWriter writer = new BufferedWriter(new FileWriter(fileName));

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

                System.out.println(offsetX + "," + offsetY);

                x = x + offsetX;
                y = y + offsetY;

                // TO-DO: write to file.
                String stringX = Double.toString(x);
                String stringY = Double.toString(y);

                try {
                    writer.write(timeStamp + "," + stringX + "," + stringY + "\n");
                    writer.flush();
                } catch (IOException e) {
                    e.printStackTrace();
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
                graphicsContext.setStroke(colorPicker.getValue());
                graphicsContext.setFill(colorPicker.getValue());
                graphicsContext.beginPath();
                graphicsContext.clearRect(before_x-RECT_RADIUS,before_y-RECT_RADIUS,before_x+RECT_RADIUS,before_y+RECT_RADIUS);
                graphicsContext.strokeRect(
                        0,              //x of the upper left corner
                        0,              //y of the upper left corner
                        canvas.getWidth(),    //width of the rectangle
                        canvas.getHeight());  //height of the rectangle
                graphicsContext.fillOval(x-RECT_RADIUS,y-RECT_RADIUS,RECT_RADIUS,RECT_RADIUS);
                before_x = x;
                before_y = y;
            }
        });
        gm.activate();
        StackPane root = new StackPane();
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
        hBox.getChildren().addAll(colorPicker, clear);
        vBox.getChildren().addAll(hBox,canvas);
        root.getChildren().add(vBox);
        Scene scene = new Scene(root, 300, 300);
        primaryStage.setTitle("Eye Gaze Visualization");
        primaryStage.setScene(scene);
        primaryStage.show();

        primaryStage.setOnCloseRequest(new EventHandler<WindowEvent>() {
            @Override
            public void handle(WindowEvent event) {
                Platform.exit();
                System.exit(0);
            }
        });
    }

    public static void main(String[] args) {
        launch(args);
    }

    private void initDraw(GraphicsContext gc){

        colorPicker = new ColorPicker(Color.BLACK);
        double canvasWidth = gc.getCanvas().getWidth();
        double canvasHeight = gc.getCanvas().getHeight();

        gc.setFill(Color.LIGHTGRAY);
        gc.setStroke(Color.BLACK);
        gc.setLineWidth(5);

        gc.fill();
        gc.strokeRect(
                0,              //x of the upper left corner
                0,              //y of the upper left corner
                canvasWidth,    //width of the rectangle
                canvasHeight);  //height of the rectangle

        gc.setStroke(Color.BLUE);
        gc.setLineWidth(LINE_WIDTH);

    }
}

from shiny import App, ui, render
import pandas as pd
import matplotlib.pyplot as plt
from io import BytesIO
from sklearn.datasets import load_iris
from seaborn import load_dataset

# RDS support
# please make sure the RDS is running on your virtual environment
try:
    import pyreadr
except ImportError:
    pyreadr = None

#sample dataset preparation
iris = load_iris(as_frame=True).frame
penguins = load_dataset("penguins").dropna()

class DataLoader:
    @staticmethod
    def load_sample_data(sample_data_name):
        """Get sample dataset by name"""
        return {
            "iris": iris.copy(),
            "penguins": penguins.copy()
        }[sample_data_name]

    @staticmethod
    def load_uploaded_data(file_info):
        """Load user-uploaded file"""
        if not file_info:
            return None
            
        file_path = file_info[0]["datapath"]
        ext = file_path.split(".")[-1].lower()
        
        if ext == "csv":
            return pd.read_csv(file_path)
        elif ext in ("xlsx", "xls"):
            return pd.read_excel(file_path)
        elif ext == "json":
            return pd.read_json(file_path)
        elif ext == "rds":
            if pyreadr is None:
                raise ImportError("RDS files require pyreadr package")
            return pyreadr.read_r(file_path)[None]
        else:
            raise ValueError(f"Unsupported file type: {ext}")

# UI
app_ui = ui.page_fluid(
    ui.h2("Data Analysis App"),
    ui.layout_sidebar(
        ui.sidebar(
            #current function
            ui.input_radio_buttons("data_source", "Data Source:",
                                  {"upload": "Upload File", "sample": "Sample Data"}),
            ui.panel_conditional(
                "input.data_source === 'upload'",
                ui.input_file("file", "Upload Data File:", 
                            accept=[".csv", ".xlsx", ".xls", ".json", ".rds"]),
                ui.markdown("Supported formats: CSV, Excel, JSON" + 
                           ("<br>RDS (requires pyreadr)" if pyreadr else ""))
            ),
            #if the user decided to use sample, here is the two sample datasets, iris and penguins
            ui.panel_conditional(
                "input.data_source === 'sample'",
                ui.input_radio_buttons("sample_data", "Choose Sample Dataset:",
                                      {"iris": "Iris Dataset", "penguins": "Penguins Dataset"})
            ),
            ui.input_select("missing", "Handle Missing Values:",
                           {"none": "None", "drop": "Remove Rows", "mean": "Fill with Mean"}),
            ui.input_select("column", "Select Column for Plot:", choices=[], multiple=False),
            width=300
        ),
        ui.output_ui("data_status"),
        ui.output_data_frame("table"),
        ui.output_text("summary"),
        ui.output_image("plot")
    ),
    #for styling purposes
    ui.tags.style("""
        body { background-color: #f8f9fa; font-family: Arial, sans-serif; }
        .sidebar { background-color: #343a40; color: white; padding: 15px; }
        .plot-container { margin-top: 20px; border: 1px solid #ddd; padding: 15px; }
        .error { color: red; font-weight: bold; }
    """)
)

#function defines
def server(input, output, session):
    def get_raw_data():
        try:
            if input.data_source() == "sample":
                return DataLoader.load_sample_data(input.sample_data())
            
            file = input.file()
            if not file:
                return None
            return DataLoader.load_uploaded_data(file)
        except Exception as e:
            return e

    def process_data():
        raw_data = get_raw_data()
        if isinstance(raw_data, Exception):
            return raw_data
        if raw_data is None:
            return None
        
        # Handle missing values
        if input.missing() == "drop":
            data = raw_data.dropna()
        elif input.missing() == "mean":
            data = raw_data.fillna(raw_data.mean(numeric_only=True))
        else:
            data = raw_data.copy()
        
        # Update column selection with default value
        numeric_cols = data.select_dtypes(include='number').columns.tolist()
        if numeric_cols:
            # Set default column to the first numeric column
            session.send_input_message("column", {
                "choices": numeric_cols,
                "value": numeric_cols[0]
            })
        else:
            session.send_input_message("column", {"choices": []})
        
        return data

    # Rest of the server functions remain the same
    @output
    @render.ui
    def data_status():
        data = get_raw_data()
        if isinstance(data, Exception):
            return ui.div(str(data), class_="error")
        return ""

    @output
    @render.data_frame
    def table():
        data = process_data()
        if isinstance(data, Exception) or data is None:
            return None
        return render.DataGrid(data, height=350)

    @output
    @render.text
    def data_summary():
        data = processed_data()
        if isinstance(data, Exception):
            return f"Error: {str(data)}"
        if data is None:
            return "No data available"
    
    # Basic dataset info
        summary = [
            f"{' Dataset Summary ':=^80}",
            f"Total Rows: {data.shape[0]}",
            f"Total Columns: {data.shape[1]}",
            f"\nNumeric Columns ({len(data.select_dtypes(include='number').columns)}):",
            ", ".join(data.select_dtypes(include='number').columns),
            f"\nCategorical Columns ({len(data.select_dtypes(exclude='number').columns)}):",
            ", ".join(data.select_dtypes(exclude='number').columns) if data.select_dtypes(exclude='number').columns else "None",
        ]

    # Descriptive statistics
        if not data.select_dtypes(include='number').empty:
            desc = data.describe().transpose()
            stats = [
                "\n\n{:=^80}".format(" Descriptive Statistics "),
                "{:<15} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10}".format(
                "Column", "Count", "Mean", "Std", "Min", "25%", "50%", "75%", "Max"
                )
            ]
        
            for col in desc.index:
                stats.append(
                    "{:<15} {:>10.2f} {:>10.2f} {:>10.2f} {:>10.2f} {:>10.2f} {:>10.2f} {:>10.2f}".format(
                        col[:14],  # Truncate long column names
                        desc.loc[col, 'count'],
                        desc.loc[col, 'mean'],
                        desc.loc[col, 'std'],
                        desc.loc[col, 'min'],
                        desc.loc[col, '25%'],
                        desc.loc[col, '50%'],
                        desc.loc[col, '75%'],
                        desc.loc[col, 'max']
                    )
                )
        
            summary += stats

        return "\n".join(summary)

    @output
    @render.image
    def plot():
        data = process_data()
        if isinstance(data, Exception) or data is None:
            return None
        
        column = input.column()
        if not column or column not in data.columns:
            return None
        
        fig, ax = plt.subplots()
        data[column].hist(bins=20, edgecolor="black", alpha=0.7, ax=ax)
        ax.set_title(f"Histogram of {column}")
        plt.close(fig)  # Close the figure to prevent memory leaks
        
        buf = BytesIO()
        fig.savefig(buf, format="png", bbox_inches="tight")
        buf.seek(0)
        return {"src": buf, "width": "600px", "height": "400px"}

app = App(app_ui, server)

if __name__ == "__main__":
    import shiny
    shiny.run_app(app)
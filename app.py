from shiny import App, ui, render, reactive
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.datasets import load_iris
from seaborn import load_dataset

# Load and prepare Iris dataset
iris_raw = load_iris(as_frame=True)
iris = iris_raw.frame.rename(columns={"target": "species"})
iris["species"] = iris["species"].map(lambda x: iris_raw.target_names[x])

# Load Penguins dataset
penguins = load_dataset("penguins").dropna()

# Precompute statistics
SAMPLE_STATS = {
    "iris": {
        "desc": iris.describe(),
        "corr": iris.select_dtypes(include='number').corr(),
        "class_dist": iris['species'].value_counts()
    },
    "penguins": {
        "desc": penguins.describe(),
        "corr": penguins.select_dtypes(include='number').corr(),
        "class_dist": penguins['species'].value_counts()
    }
}

class DataLoader:
    @staticmethod
    def load_sample_data(name):
        return {
            "iris": iris.copy(),
            "penguins": penguins.copy()
        }[name]

    @staticmethod
    def load_uploaded_data(file_info):
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
        else:
            raise ValueError(f"Unsupported file type: {ext}")

app_ui = ui.page_fluid(
    ui.h2("🌸 Iris & 🐧 Penguins Explorer"),
    ui.layout_sidebar(
        ui.panel_sidebar(
            ui.input_radio_buttons("data_source", "Data Source:",
                                  {"upload": "Upload File", "sample": "Sample Data"}),
            ui.panel_conditional(
                "input.data_source === 'upload'",
                ui.input_file("file", "Choose File:", 
                            accept=[".csv", ".xlsx", ".xls", ".json"]),
            ),
            ui.panel_conditional(
                "input.data_source === 'sample'",
                ui.input_radio_buttons("sample_data", "Dataset:",
                                      {"iris": "Iris", "penguins": "Penguins"})
            ),
            ui.input_select("missing", "Handle Missing Values:",
                           {"none": "None", "drop": "Remove Rows", "mean": "Fill Mean"}),
            ui.input_select("plot_type", "Visualization Type:",
                           {"dist": "Distribution", "box": "Box Plot",
                            "scatter": "Scatter Plot", "corr": "Correlation"}),
            ui.panel_conditional(
                "input.plot_type === 'scatter'",
                ui.input_select("x_axis", "X Axis:", choices=[]),
                ui.input_select("y_axis", "Y Axis:", choices=[]),
            ),
            width=300
        ),
        ui.panel_main(
            ui.navset(
                ui.nav("Data", ui.output_data_frame("data_table")),
                ui.nav("Stats", ui.output_ui("data_summary")),
                ui.nav("Plots", ui.output_plot("main_plot")),
                ui.nav("Insights", ui.output_ui("data_insights"))
            )
        )
    )
)

def server(input, output, session):
    @reactive.Calc
    def processed_data():
        try:
            if input.data_source() == "sample":
                data = DataLoader.load_sample_data(input.sample_data())
            else:
                file = input.file()
                data = DataLoader.load_uploaded_data(file) if file else None
            
            if data is None or not isinstance(data, pd.DataFrame):
                return None

            # Handle missing values
            if input.missing() == "drop":
                return data.dropna()
            if input.missing() == "mean":
                return data.fillna(data.select_dtypes(include='number').mean())
            return data
            
        except Exception as e:
            return str(e)

    @reactive.Effect
    def update_axes():
        data = processed_data()
        if isinstance(data, pd.DataFrame):
            numeric_cols = data.select_dtypes(include='number').columns.tolist()
            session.send_input_message("x_axis", {"choices": numeric_cols})
            session.send_input_message("y_axis", {"choices": numeric_cols})

    @output
    @render.data_frame
    def data_table():
        data = processed_data()
        if isinstance(data, pd.DataFrame):
            return render.DataGrid(data, filters=True, height=350)

    @output
    @render.ui
    def data_summary():
        data = processed_data()
        if not isinstance(data, pd.DataFrame):
            return ui.markdown("No data available")
        
        stats = SAMPLE_STATS[input.sample_data()] if input.data_source() == "sample" else {
            "desc": data.describe(),
            "corr": data.select_dtypes(include='number').corr(),
            "class_dist": data.select_dtypes(exclude='number').value_counts()
        }

        return ui.TagList(
            ui.h4("Dataset Summary"),
            ui.markdown(f"""
                **Rows:** {data.shape[0]}  
                **Columns:** {data.shape[1]}  
                **Numeric Features:** {len(data.select_dtypes(include='number').columns)}  
                **Categorical Features:** {len(data.select_dtypes(exclude='number').columns)}
            """),
            ui.h5("Descriptive Statistics"),
            ui.HTML(stats["desc"].to_html(classes="table table-sm")),
            ui.h5("Class Distribution"),
            ui.HTML(stats["class_dist"].to_frame().to_html(classes="table table-sm"))
        )

    @output
    @render.plot
    def main_plot():
        data = processed_data()
        if not isinstance(data, pd.DataFrame):
            return
        
        plot_type = input.plot_type()
        fig, ax = plt.subplots(figsize=(10, 6))
        
        try:
            if plot_type == "dist":
                sns.histplot(data[input.x_axis()], kde=True, ax=ax)
                ax.set_title(f"Distribution of {input.x_axis()}")
                
            elif plot_type == "box":
                if "species" in data.columns:
                    sns.boxplot(data=data, x="species", y=input.y_axis(), ax=ax)
                    ax.set_title(f"{input.y_axis()} by Species")
                else:
                    sns.boxplot(data=data[input.y_axis()], ax=ax)
                    ax.set_title(f"Box Plot of {input.y_axis()}")
                    
            elif plot_type == "scatter":
                hue = "species" if "species" in data.columns else None
                sns.scatterplot(data=data, x=input.x_axis(), y=input.y_axis(), 
                              hue=hue, ax=ax)
                ax.set_title(f"{input.y_axis()} vs {input.x_axis()}")
                
            elif plot_type == "corr":
                corr = data.select_dtypes(include='number').corr()
                sns.heatmap(corr, annot=True, fmt=".2f", cmap='coolwarm', ax=ax)
                ax.set_title("Correlation Matrix")
            
            plt.tight_layout()
            return fig
        finally:
            plt.close(fig)

    @output
    @render.ui
    def data_insights():
        data = processed_data()
        if not isinstance(data, pd.DataFrame):
            return
        
        insights = []
        if input.data_source() == "sample":
            if input.sample_data() == "iris":
                insights.append(ui.markdown("""
                    ### Iris Dataset Insights
                    - Contains 3 iris species: setosa, versicolor, and virginica
                    - 50 samples per species (balanced)
                    - Strong correlation between petal measurements
                """))
            else:
                insights.append(ui.markdown("""
                    ### Penguins Dataset Insights
                    - Contains 3 penguin species
                    - Includes island and sex information
                    - Bill dimensions vary between species
                """))
        
        return ui.div(*insights, class_="p-3 bg-light rounded")

app = App(app_ui, server)

if __name__ == "__main__":
    import shiny
    shiny.run_app(app)
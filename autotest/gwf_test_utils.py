PLOT_UZR_TESTS = False  # flag to control plotting of UZR results, when not CI


def get_uzr_soil_data(record_name):

    record = {}
    match record_name:
        # The Haverkamp data from the article,
        # converted to match our parametrization
        case "Celia1990-eq10-Haverkamp":
            record["porosity"] = 0.287
            record["satres"] = 0.26132  # 0.075 / porosity
            record["alpha"] = 0.027074  # = exp(ln(1./1.611e+06)/3.96)
            record["n"] = 3.96
            record["beta"] = 0.052408  # = exp(ln(1./1.175e+06)/4.74)
            record["k"] = 4.74

    return record

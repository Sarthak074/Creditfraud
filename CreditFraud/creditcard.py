import streamlit as st
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import plotly.express as px
import joblib
from sklearn.ensemble import RandomForestClassifier

# Streamlit UI
st.set_page_config(page_title="Fraud Detection with COBOL + ML", layout="centered")
st.title("💳 Fraud Detection with COBOL + ML")

uploaded_file = st.file_uploader("📄 Upload your FRAUD.INPUT.txt", type=["txt"])

if uploaded_file:
    input_path = "FRAUD.INPUT.txt"
    with open(input_path, "wb") as f:
        f.write(uploaded_file.read())
    st.success("✅ File uploaded successfully.")

    # COBOL source
    cobol_code = """IDENTIFICATION DIVISION.
       PROGRAM-ID. FRAUDCHK.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO 'FRAUD.INPUT.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO 'FRAUD.OUTPUT.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  INFILE.
       01  IN-RECORD              PIC X(80).

       FD  OUTFILE.
       01  OUT-RECORD             PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-TRANS-ID            PIC X(10).
       01  WS-AMOUNT-STRING       PIC X(15).
       01  WS-DATE                PIC X(8).
       01  WS-LOCATION            PIC X(20).

       01  WS-AMOUNT-NUMERIC      PIC 9(7)V99 COMP-3.

       01  WS-TEMP-AMOUNT         PIC X(15).

       01  EOF-FLAG               PIC X VALUE 'N'.
           88  END-OF-FILE        VALUE 'Y'.
           88  NOT-END-OF-FILE    VALUE 'N'.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           OPEN INPUT INFILE
                OUTPUT OUTFILE

           PERFORM UNTIL END-OF-FILE
               READ INFILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM CHECK-AND-WRITE
               END-READ
           END-PERFORM

           CLOSE INFILE OUTFILE
           STOP RUN.

       CHECK-AND-WRITE.
           IF IN-RECORD(1:3) = "TXN"
               MOVE IN-RECORD(1:10)   TO WS-TRANS-ID
               MOVE IN-RECORD(12:15)  TO WS-AMOUNT-STRING
               MOVE IN-RECORD(28:8)   TO WS-DATE
               MOVE IN-RECORD(37:20)  TO WS-LOCATION

               PERFORM CLEAN-AMOUNT

               IF WS-AMOUNT-NUMERIC > 100000.00
                   MOVE IN-RECORD TO OUT-RECORD
                   WRITE OUT-RECORD
               END-IF
           END-IF.

       CLEAN-AMOUNT.
           MOVE WS-AMOUNT-STRING TO WS-TEMP-AMOUNT
           UNSTRING WS-TEMP-AMOUNT DELIMITED BY ALL "."
               INTO WS-TEMP-AMOUNT
           END-UNSTRING
           MOVE FUNCTION NUMVAL(WS-TEMP-AMOUNT) TO WS-AMOUNT-NUMERIC.
"""  # (Same COBOL source you had – keep it unchanged)
    with open("fraudchk.cob", "w") as f:
        f.write(cobol_code)

    st.info("🔧 Compiling COBOL code...")
    os.system("cobc -x fraudchk.cob -o fraudchk.out")
    os.system("./fraudchk.out")

    # Read data
    with open("FRAUD.INPUT.txt", "r") as f:
        input_lines = [line.strip() for line in f if line.strip()]
    with open("FRAUD.OUTPUT.txt", "r") as f:
        output_lines = [line.strip() for line in f if line.strip()]

    total_transactions = len(input_lines)
    fraud_transactions = len(output_lines)

    st.subheader("📊 Fraud Summary")
    st.write(f"**Total Transactions:** {total_transactions}")
    st.write(f"**Detected Possible Frauds:** {fraud_transactions}")

    # Animated plotly pie chart
    fig = px.pie(
        names=["Non-Fraud", "Possible Fraud"],
        values=[total_transactions - fraud_transactions, fraud_transactions],
        color_discrete_sequence=["#a3e4d7", "#f1948a"],
        title="Fraud Distribution"
    )
    st.plotly_chart(fig)

    if fraud_transactions > 0:
        st.subheader("🔍 COBOL Detected Possible Frauds")
        for line in output_lines:
            st.code(line, language="txt")
    else:
        st.info("No suspicious transactions detected by COBOL.")

    # Extract features
    def parse_line(line):
        try:
            amt_str = line[11:26].replace(",", "").strip()
            amt = float(amt_str) if amt_str else 0.0
        except ValueError:
            amt = 0.0

        try:
            date = line[27:35]
            day = int(date[6:8]) if len(date) >= 8 else 1
        except:
            day = 1

        location = line[36:56].strip()
        loc_len = len(location)

        return {"amt": amt, "day": day, "loc_len": loc_len, "loc": location}

    fraud_df = pd.DataFrame([parse_line(l) for l in output_lines if l.strip()])
    input_df = pd.DataFrame([parse_line(l) for l in input_lines if l.strip()])

    st.subheader("🤖 Machine Learning-based Classification")

    # If model doesn't exist, train a dummy one
    if not os.path.exists("fraud_model.pkl"):
        st.info("🔁 Training dummy ML model...")
        X = np.random.rand(100, 3) * [500000, 30, 20]
        y = np.random.randint(0, 2, 100)
        model = RandomForestClassifier()
        model.fit(X, y)
        joblib.dump(model, "fraud_model.pkl")

    model = joblib.load("fraud_model.pkl")

    # Known cities from input set
    known_cities = set(input_df["loc"].value_counts().head(10).index)

    # ML Prediction
    def classify_fraud(row):
        x = [row["amt"], row["day"], row["loc_len"]]
        base_prediction = model.predict([x])[0]

        # Boost prediction if city is unseen
        if row["loc"] not in known_cities:
            if base_prediction == 0:
                base_prediction = 1
        return base_prediction

    fraud_df["ml_pred"] = fraud_df.apply(classify_fraud, axis=1)

    real_frauds = fraud_df[fraud_df["ml_pred"] == 1]
    false_alarms = fraud_df[fraud_df["ml_pred"] == 0]

    st.success(f"✅ ML says {len(real_frauds)} are actual frauds, {len(false_alarms)} are likely false positives")

    if not real_frauds.empty:
        st.subheader("✅ Confirmed Frauds (via ML):")
        for i, row in real_frauds.iterrows():
            st.code(output_lines[i], language="txt")

    if not false_alarms.empty:
        st.subheader("❌ False Positives (ML thinks these aren't frauds):")
        for i, row in false_alarms.iterrows():
            st.code(output_lines[i], language="txt")

    # Additional Plots
    st.subheader("📈 Feature Insights")

    # Bar: Top 10 Locations
    loc_counts = input_df["loc"].value_counts().head(10)
    fig2 = px.bar(
        loc_counts,
        x=loc_counts.index,
        y=loc_counts.values,
        title="Top 10 Transaction Locations",
        labels={"x": "Location", "y": "Count"},
        color=loc_counts.values,
        color_continuous_scale="Teal"
    )
    st.plotly_chart(fig2)

import os
import fitz
import re
import json
import numpy as np
from tqdm import tqdm
from sentence_transformers import SentenceTransformer
from sklearn.metrics.pairwise import cosine_similarity

# settinfs

PDF_FOLDER = r"E:/files"
OUTPUT_JSONL = r"E:/files2/qwen_dataset.jsonl"

MAX_CHARS = 12000
MIN_CHARS = 200
TOP_K = 3

# prompt
SYSTEM_PROMPT = (
    "Ты эксперт по кибербезопасности. "
    "Определи 1–5 наиболее подходящих тегов по тексту статьи. "
    "Отвечай строго тегами через | без пояснений."
)


TAGS = [

"Симметричное шифрование (AES, блочные шифры)",
"Асимметричное шифрование, PKI",
"Постквантовая криптография",
"Zero-knowledge proofs / доказательства с нулевым разглашением",
"Гомоморфное шифрование",
"Протоколы многостороннего вычисления (MPC)",

"Обнаружение вторжений (IDS/IPS)",
"DDoS, сетевые атаки",
"Firewall, VPN, SD-WAN",

"Adversarial attacks",
"Отравление данных (data poisoning)",
"Differential privacy",
"Federated learning + privacy",

"Fuzzing, статический анализ",
"Side-channel attacks (утечки через кэш, питание и т.д.)",
"Эксплойты, CVE-анализ",
"Prompt injection / LLM-атаки",

"Анонимизация данных",
"GDPR / соответствие нормативам",
"Приватность в IoT",

"Smart contract security",
"Консенсус-протоколы",
"DeFi-атаки",

"TEE / доверенное исполнение (SGX, TrustZone)",
"ОС-безопасность, гипервизоры",
"Hardware security",

"IoT / CPS / автономные системы",
"Промышленные системы (ICS/SCADA)",
"Биометрия"
]

# embending

model = SentenceTransformer("sentence-transformers/all-MiniLM-L6-v2")
tag_emb = model.encode(TAGS, normalize_embeddings=True)

# pdf extraction

def extract_text(path):
    try:
        doc = fitz.open(path)
        text = ""

        for page in doc:
            text += page.get_text()

        text = re.sub(r"\s+", " ", text).strip()

        if len(text) < MIN_CHARS:
            return None

        return text[:MAX_CHARS]

    except Exception:
        return None


# tagsemb

def get_tags(text):
    emb = model.encode([text], normalize_embeddings=True)
    sims = cosine_similarity(emb, tag_emb)[0]

    idx = sims.argsort()[-TOP_K:][::-1]

    return [TAGS[i] for i in idx]





pdf_files = [
    f for f in os.listdir(PDF_FOLDER)
    if f.endswith(".pdf")
]

print("PDF FOUND:", len(pdf_files))

written = 0
skipped = 0

with open(OUTPUT_JSONL, "w", encoding="utf-8") as f:

    for file in tqdm(pdf_files):

        path = os.path.join(PDF_FOLDER, file)
        text = extract_text(path)

        if text is None:
            skipped += 1
            continue

        tags = get_tags(text)

        sample = {
            "messages": [
                {
                    "role": "system",
                    "content": SYSTEM_PROMPT
                },
                {
                    "role": "user",
                    "content": text
                },
                {
                    "role": "assistant",
                    "content": "|".join(tags)
                }
            ]
        }

        f.write(json.dumps(sample, ensure_ascii=False) + "\n")
        written += 1

print("\nDONE")
print("Written:", written)
print("Skipped:", skipped)
print("Output:", OUTPUT_JSONL)
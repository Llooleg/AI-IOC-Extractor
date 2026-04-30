import json
import requests
import time
from pathlib import Path

# settings

LM_STUDIO_URL = "http://localhost:1234/v1/chat/completions"
MODEL_NAME = "tokenizer_qwen3-8b.Q6_K"

INPUT_FILE = r"E:\metadata.json"
OUTPUT_FILE  = r"E:\output_gguf23_gguf\json\tagged.json"

BATCH_SIZE = 50
MAX_RETRIES = 3

ALLOWED_TAGS = [
    "Симметричное шифрование (AES, блочные шифры)",
    "Асимметричное шифрование, PKI",
    "Постквантовая криптография",
    "Zero-knowledge proofs",
    "Гомоморфное шифрование",
    "Протоколы MPC",
    "Обнаружение вторжений (IDS/IPS)",
    "DDoS, сетевые атаки",
    "Firewall, VPN, SD-WAN",
    "Adversarial attacks",
    "Отравление данных (data poisoning)",
    "Differential privacy",
    "Federated learning + privacy",
    "Fuzzing, статический анализ",
    "Side-channel attacks",
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
    "Биометрия",
]

# prompt

def build_prompt(abstract: str) -> str:
    tags_list = "\n".join(f"- {t}" for t in ALLOWED_TAGS)
    return (
        f"/no_think\n"
        f"You are a scientific article classifier. "
        f"Assign exactly one tag from the following list to the article abstract.\n\n"
        f"Allowed tags:\n{tags_list}\n\n"
        f"Abstract:\n{abstract}\n\n"
        f"Reply with only the tag name, nothing else."
    )

# lmst

def get_tag(abstract: str) -> str | None:
    payload = {
        "model": MODEL_NAME,
        "messages": [
            {"role": "user", "content": build_prompt(abstract)}
        ],
        "temperature": 0.0,
        "max_tokens": 30,
    }

    for attempt in range(MAX_RETRIES):
        try:
            resp = requests.post(LM_STUDIO_URL, json=payload, timeout=60)
            resp.raise_for_status()
            raw = resp.json()["choices"][0]["message"]["content"].strip()

            for allowed in ALLOWED_TAGS:
                if raw.lower() == allowed.lower():
                    return allowed

            for allowed in ALLOWED_TAGS:
                if allowed.lower() in raw.lower():
                    return allowed

            print(f"  Неизвестный тег: '{raw}' — None")
            return None

        except requests.exceptions.Timeout:
            print(f"  Таймаут (попытка {attempt + 1}/{MAX_RETRIES})")
            time.sleep(2)
        except Exception as e:
            print(f"  Ошибка: {e} (попытка {attempt + 1}/{MAX_RETRIES})")
            time.sleep(2)

    return None

# loadsave

def load_articles(path: str) -> list[dict]:
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)

def load_existing_results(path: str) -> dict:
    if Path(path).exists():
        try:
            with open(path, "r", encoding="utf-8") as f:
                content = f.read().strip()
            if not content:
                return {}
            results = json.loads(content)
            return {item["id"]: item["tag"] for item in results}
        except (json.JSONDecodeError, KeyError):
            print("  Файл результатов повреждён или пуст — начинаем заново")
            return {}
    return {}

def save_results(results: dict, path: str):
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    output = [{"id": k, "tag": v} for k, v in results.items()]
    with open(path, "w", encoding="utf-8") as f:
        json.dump(output, f, ensure_ascii=False, indent=2)

# mainc

def main():
    print("Загружаем статьи...")
    articles = load_articles(INPUT_FILE)
    print(f"  Всего статей: {len(articles)}")

    existing = load_existing_results(OUTPUT_FILE)
    print(f"  Уже обработано: {len(existing)}\n")

    results = dict(existing)
    to_process = [a for a in articles if str(a["id"]) not in results]
    total = len(to_process)
    print(f"Осталось: {total}\n")

    for i, article in enumerate(to_process, 1):
        article_id = str(article["id"])
        abstract = article.get("abstract", "").strip()

        if not abstract:
            print(f"[{i}/{total}] ID {article_id} — пустой абстракт, пропускаем")
            results[article_id] = None
            continue

        tag = get_tag(abstract)
        results[article_id] = tag

        status = tag if tag else "None"
        print(f"[{i}/{total}] ID {article_id} -> {status}")

        if i % BATCH_SIZE == 0:
            save_results(results, OUTPUT_FILE)
            print(f"  Сохранено {len(results)}\n")

    save_results(results, OUTPUT_FILE)

    tagged = sum(1 for v in results.values() if v is not None)
    print(f"\nГотово! Всего: {len(results)}, с тегом: {tagged}, без: {len(results) - tagged}")
    print(f"{OUTPUT_FILE}")

if __name__ == "__main__":
    main()
"""Vẽ Hình 1 (mô hình đề xuất) và Hình 2 (kết quả SEM) của bài HTCTTL-02 từ file cấu hình YAML.

Chạy: python make_paper_figures.py   (trong thư mục chứa file này)
Cần: sem_block_proposed.yaml, sem_block_results.yaml (cùng thư mục) và sem_diagrammer.py
(skill sem-block-diagram; tự tìm ở _shared/ khi chạy trên Colab hoặc ở .agents/ khi chạy local).
Lưu ý: các hệ số trong YAML kết quả do tác giả nhập từ kết quả chạy analysis_sem.R (bảng T2/T6),
không đọc trực tiếp từ file CSV; hãy đối chiếu khi chạy lại phân tích.
"""
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REL = Path("sem-block-diagram/scripts/sem_diagrammer.py")


def find_helper() -> Path:
    cands = [HERE / "../../_shared/agents/skills" / REL]  # gói Colab
    for p in [HERE, *HERE.parents]:                       # chạy local trong repo
        cands.append(p / ".agents/skills" / REL)
    for c in cands:
        if c.is_file():
            return c.resolve()
    sys.exit("Không tìm thấy sem_diagrammer.py: " + ", ".join(str(c) for c in cands[:2]))


helper = find_helper()
for cfg in ("sem_block_proposed.yaml", "sem_block_results.yaml"):
    print("Vẽ:", cfg)
    subprocess.run([sys.executable, str(helper), "--config", cfg], cwd=HERE, check=True)
print("Xong.")

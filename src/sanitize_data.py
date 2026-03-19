#!/usr/bin/env python3
"""Generate schema-only synthetic survey files from real survey extracts.

This script mirrors an input directory into an output directory while removing
row-level data from supported tabular formats and preserving metadata where
possible.
"""

from __future__ import annotations

import argparse
import csv
import importlib
import re
import shutil
import sys
from collections import Counter
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple


ALLOWED_EXTENSIONS = {
    ".dta",
    ".sav",
    ".csv",
    ".xlsx",
    ".pdf",
    ".txt",
    ".do",
    ".docx",
    ".sas",
    ".sps",
    ".zip",
}

COPY_UNCHANGED_EXTENSIONS = {".pdf", ".txt", ".do", ".docx"}
SKIP_EXTENSIONS = {".zip"}


@dataclass
class FileResult:
    relative_path: str
    extension: str
    action: str
    status: str
    message: str


def parse_args() -> argparse.Namespace:
    project_root = Path(__file__).resolve().parents[1]
    default_input = project_root / "data"
    default_output = project_root / "data-sanitized"

    parser = argparse.ArgumentParser(
        description=(
            "Create schema-only synthetic files from survey data while keeping "
            "filenames, folder structure, columns, and labels."
        )
    )
    parser.add_argument(
        "--input-dir",
        type=Path,
        default=default_input,
        help=f"Input directory (default: {default_input})",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=default_output,
        help=f"Output directory (default: {default_output})",
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Overwrite output directory if it already exists.",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Plan actions without writing files.",
    )
    return parser.parse_args()


def ensure_output_dir(output_dir: Path, overwrite: bool, dry_run: bool) -> None:
    if output_dir.exists() and output_dir.is_file():
        raise ValueError(f"Output path is a file: {output_dir}")

    if output_dir.exists() and any(output_dir.iterdir()):
        if not overwrite:
            raise ValueError(
                f"Output directory already exists and is not empty: {output_dir}. "
                "Use --overwrite to replace it."
            )
        if not dry_run:
            shutil.rmtree(output_dir)

    if not dry_run:
        output_dir.mkdir(parents=True, exist_ok=True)


def iter_files(root: Path) -> Iterable[Path]:
    for path in sorted(root.rglob("*")):
        if path.is_file():
            yield path


def safe_decode(raw: bytes) -> Tuple[str, str]:
    for encoding in ("utf-8-sig", "utf-8", "latin-1"):
        try:
            return raw.decode(encoding), encoding
        except UnicodeDecodeError:
            continue
    return raw.decode("latin-1", errors="ignore"), "latin-1"


def copy_file(src: Path, dst: Path, dry_run: bool) -> None:
    if dry_run:
        return
    dst.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(src, dst)


def write_text(dst: Path, content: str, dry_run: bool) -> None:
    if dry_run:
        return
    dst.parent.mkdir(parents=True, exist_ok=True)
    dst.write_text(content, encoding="utf-8")


def sanitize_csv(src: Path, dst: Path, dry_run: bool) -> str:
    raw = src.read_bytes()
    text, _ = safe_decode(raw)

    sample = text[:65536]
    if not sample.strip():
        write_text(dst, "", dry_run)
        return "empty input"

    try:
        dialect = csv.Sniffer().sniff(sample, delimiters=",;\t|")
    except csv.Error:
        dialect = csv.excel

    rows = list(csv.reader(text.splitlines(), dialect=dialect))
    header = rows[0] if rows else []

    if dry_run:
        return f"columns={len(header)}"

    dst.parent.mkdir(parents=True, exist_ok=True)
    with dst.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.writer(
            handle,
            delimiter=dialect.delimiter,
            quotechar=dialect.quotechar,
            escapechar=dialect.escapechar,
            doublequote=dialect.doublequote,
            quoting=dialect.quoting,
            lineterminator="\n",
        )
        if header:
            writer.writerow(header)
    return f"columns={len(header)}, delimiter={repr(dialect.delimiter)}"


def sanitize_xlsx(src: Path, dst: Path, dry_run: bool) -> str:
    try:
        pd = importlib.import_module("pandas")
    except ImportError as exc:
        raise RuntimeError("pandas is required for .xlsx processing") from exc

    xls = pd.ExcelFile(src)
    sheet_headers: Dict[str, List[str]] = {}
    for sheet_name in xls.sheet_names:
        header_df = pd.read_excel(src, sheet_name=sheet_name, nrows=0)
        sheet_headers[sheet_name] = header_df.columns.tolist()

    if dry_run:
        return f"sheets={len(sheet_headers)}"

    dst.parent.mkdir(parents=True, exist_ok=True)
    with pd.ExcelWriter(dst, engine="openpyxl") as writer:
        for sheet_name, columns in sheet_headers.items():
            pd.DataFrame(columns=columns).to_excel(writer, sheet_name=sheet_name, index=False)
    return f"sheets={len(sheet_headers)}"


def _convert_column_labels(meta, column_names: List[str]) -> Dict[str, str]:
    if hasattr(meta, "column_names_to_labels") and meta.column_names_to_labels:
        return dict(meta.column_names_to_labels)

    labels = getattr(meta, "column_labels", None)
    if not labels:
        return {}

    out: Dict[str, str] = {}
    for idx, name in enumerate(column_names):
        label = labels[idx] if idx < len(labels) else None
        if label is not None:
            out[name] = str(label)
    return out


def sanitize_dta(src: Path, dst: Path, dry_run: bool) -> str:
    try:
        pd = importlib.import_module("pandas")
        pyreadstat = importlib.import_module("pyreadstat")
    except ImportError as exc:
        raise RuntimeError("pyreadstat and pandas are required for .dta processing") from exc

    sample_df, meta = pyreadstat.read_dta(src, row_limit=1)
    column_names = list(meta.column_names)
    if sample_df.empty:
        empty_df = pd.DataFrame(columns=column_names)
    else:
        empty_df = sample_df.iloc[0:0].copy()

    column_labels = _convert_column_labels(meta, column_names)
    variable_value_labels = getattr(meta, "variable_value_labels", None) or None
    variable_format = getattr(meta, "original_variable_types", None) or None
    file_label = getattr(meta, "file_label", None) or ""

    if dry_run:
        return f"columns={len(column_names)}"

    dst.parent.mkdir(parents=True, exist_ok=True)
    pyreadstat.write_dta(
        empty_df,
        str(dst),
        file_label=file_label,
        column_labels=column_labels,
        variable_value_labels=variable_value_labels,
        variable_format=variable_format,
    )
    return f"columns={len(column_names)}"


def sanitize_sav(src: Path, dst: Path, dry_run: bool) -> str:
    try:
        pd = importlib.import_module("pandas")
        pyreadstat = importlib.import_module("pyreadstat")
    except ImportError as exc:
        raise RuntimeError("pyreadstat and pandas are required for .sav processing") from exc

    sample_df, meta = pyreadstat.read_sav(src, row_limit=1)
    column_names = list(meta.column_names)
    if sample_df.empty:
        empty_df = pd.DataFrame(columns=column_names)
    else:
        empty_df = sample_df.iloc[0:0].copy()

    column_labels = _convert_column_labels(meta, column_names)
    variable_value_labels = getattr(meta, "variable_value_labels", None) or None
    variable_format = getattr(meta, "original_variable_types", None) or None
    file_label = getattr(meta, "file_label", None) or ""
    variable_measure = getattr(meta, "variable_measure", None) or None
    variable_display_width = getattr(meta, "variable_display_width", None) or None

    if dry_run:
        return f"columns={len(column_names)}"

    dst.parent.mkdir(parents=True, exist_ok=True)
    pyreadstat.write_sav(
        empty_df,
        str(dst),
        file_label=file_label,
        column_labels=column_labels,
        variable_value_labels=variable_value_labels,
        variable_format=variable_format,
        variable_measure=variable_measure,
        variable_display_width=variable_display_width,
    )
    return f"columns={len(column_names)}"


def sanitize_syntax_with_embedded_data(src: Path, dst: Path, dry_run: bool, ext: str) -> str:
    text, _ = safe_decode(src.read_bytes())

    if ext == ".sps":
        cleaned, removed_rows = _strip_spss_data_blocks(text)
    else:
        cleaned, removed_rows = _strip_sas_data_blocks(text)

    write_text(dst, cleaned, dry_run)
    return f"removed_rows={removed_rows}"


def _strip_spss_data_blocks(text: str) -> Tuple[str, int]:
    lines = text.splitlines(keepends=True)
    begin_re = re.compile(r"^\s*BEGIN\s+DATA\b", re.IGNORECASE)
    end_re = re.compile(r"^\s*END\s+DATA\b", re.IGNORECASE)

    out: List[str] = []
    in_data = False
    removed = 0

    for line in lines:
        if not in_data and begin_re.search(line):
            in_data = True
            out.append(line)
            continue

        if in_data and end_re.search(line):
            in_data = False
            out.append(line)
            continue

        if in_data:
            if line.strip():
                removed += 1
            continue

        out.append(line)

    return "".join(out), removed


def _strip_sas_data_blocks(text: str) -> Tuple[str, int]:
    lines = text.splitlines(keepends=True)
    start_re = re.compile(r"^\s*(datalines4?|cards4?)\s*;", re.IGNORECASE)

    out: List[str] = []
    in_data = False
    terminator = ";"
    removed = 0

    for line in lines:
        if not in_data:
            match = start_re.search(line)
            if match:
                keyword = match.group(1).lower()
                terminator = ";;;;" if keyword.endswith("4") else ";"
                in_data = True
                out.append(line)
                continue
            out.append(line)
            continue

        stripped = line.strip()
        if stripped == terminator:
            in_data = False
            out.append(line)
            continue

        if stripped:
            removed += 1

    return "".join(out), removed


def write_error_csv(output_dir: Path, results: List[FileResult], dry_run: bool) -> Optional[Path]:
    errors = [r for r in results if r.status in {"error", "unknown"}]
    if not errors:
        return None

    error_path = output_dir / "_synthetic_errors.csv"
    if dry_run:
        return error_path

    error_path.parent.mkdir(parents=True, exist_ok=True)
    with error_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow(["relative_path", "extension", "action", "status", "message"])
        for r in errors:
            writer.writerow([r.relative_path, r.extension, r.action, r.status, r.message])
    return error_path


def print_summary(results: List[FileResult], error_csv: Optional[Path]) -> None:
    total = len(results)
    status_counts = Counter(r.status for r in results)
    action_counts = Counter(r.action for r in results)

    print("Run summary")
    print(f"- files seen: {total}")
    print(f"- success: {status_counts.get('success', 0)}")
    print(f"- skipped: {status_counts.get('skipped', 0)}")
    print(f"- unknown: {status_counts.get('unknown', 0)}")
    print(f"- errors: {status_counts.get('error', 0)}")
    print("- actions:")
    for action, count in sorted(action_counts.items()):
        print(f"  - {action}: {count}")
    if error_csv is not None:
        print(f"- error details: {error_csv}")


def process_file(src: Path, input_dir: Path, output_dir: Path, dry_run: bool) -> FileResult:
    rel = src.relative_to(input_dir)
    dst = output_dir / rel
    ext = src.suffix.lower()

    if ext not in ALLOWED_EXTENSIONS:
        return FileResult(str(rel), ext or "<noext>", "unknown_extension", "unknown", "not in allowlist")

    if ext in SKIP_EXTENSIONS:
        return FileResult(str(rel), ext, "skip", "skipped", "ignored by policy")

    if dry_run:
        if ext in COPY_UNCHANGED_EXTENSIONS:
            return FileResult(str(rel), ext, "copy", "success", "planned copy unchanged")
        if ext == ".csv":
            return FileResult(str(rel), ext, "sanitize_csv", "success", "planned header-only output")
        if ext == ".xlsx":
            return FileResult(str(rel), ext, "sanitize_xlsx", "success", "planned sheet-header-only output")
        if ext == ".dta":
            return FileResult(str(rel), ext, "sanitize_dta", "success", "planned schema-only output")
        if ext == ".sav":
            return FileResult(str(rel), ext, "sanitize_sav", "success", "planned schema-only output")
        if ext in {".sas", ".sps"}:
            return FileResult(str(rel), ext, "sanitize_syntax", "success", "planned embedded-row removal")

    try:
        if ext in COPY_UNCHANGED_EXTENSIONS:
            copy_file(src, dst, dry_run)
            return FileResult(str(rel), ext, "copy", "success", "copied unchanged")

        if ext == ".csv":
            msg = sanitize_csv(src, dst, dry_run)
            return FileResult(str(rel), ext, "sanitize_csv", "success", msg)

        if ext == ".xlsx":
            msg = sanitize_xlsx(src, dst, dry_run)
            return FileResult(str(rel), ext, "sanitize_xlsx", "success", msg)

        if ext == ".dta":
            msg = sanitize_dta(src, dst, dry_run)
            return FileResult(str(rel), ext, "sanitize_dta", "success", msg)

        if ext == ".sav":
            msg = sanitize_sav(src, dst, dry_run)
            return FileResult(str(rel), ext, "sanitize_sav", "success", msg)

        if ext in {".sas", ".sps"}:
            msg = sanitize_syntax_with_embedded_data(src, dst, dry_run, ext)
            return FileResult(str(rel), ext, "sanitize_syntax", "success", msg)

        return FileResult(str(rel), ext, "unhandled", "unknown", "extension allowlisted but no handler")
    except Exception as exc:
        return FileResult(str(rel), ext or "<noext>", "process", "error", str(exc))


def main() -> int:
    args = parse_args()
    input_dir = args.input_dir.resolve()
    output_dir = args.output_dir.resolve()

    if not input_dir.exists() or not input_dir.is_dir():
        print(f"Input directory not found: {input_dir}", file=sys.stderr)
        return 2

    try:
        ensure_output_dir(output_dir, overwrite=args.overwrite, dry_run=args.dry_run)
    except ValueError as exc:
        print(str(exc), file=sys.stderr)
        return 2

    results: List[FileResult] = []
    for src in iter_files(input_dir):
        results.append(process_file(src, input_dir, output_dir, dry_run=args.dry_run))

    error_csv = write_error_csv(output_dir, results, dry_run=args.dry_run)
    print_summary(results, error_csv)

    has_errors = any(r.status == "error" for r in results)
    return 1 if has_errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
